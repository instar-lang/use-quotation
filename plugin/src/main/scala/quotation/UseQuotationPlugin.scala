package quotation
import dotty.tools.dotc.ast.tpd

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report

class UseQuotationPlugin extends StandardPlugin:
  val name = "use-quotation"
  val description = "Two-stage quotation combinator elaboration"
  override def initialize(options: List[String])(using Context): List[PluginPhase] = List(new ElaborationPhase)

class ElaborationPhase extends PluginPhase:
  import tpd.*
  val phaseName = "quotationElaboration"
  override val runsAfter = Set("staging")
  override val runsBefore = Set("splicing")

  private def exprAny(using Context): Type = defn.QuotedExprClass.typeRef.appliedTo(defn.AnyType)

  // Reject level 2 before rewriting removes the original quotation structure.
  override def prepareForApply(tree: Apply)(using Context): Context =
    tree match
      case Apply(Select(q: Quote, _), _) if !q.isTypeQuote =>
        val checker = new TreeTraverser:
          private var level = 1
          def traverse(tree: Tree)(using Context): Unit = tree match
            case quote: Quote =>
              if level >= 1 then report.error("use-quotation supports only two stages: nested quotation is unsupported", quote.srcPos)
              level += 1
              traverse(quote.body)
              level -= 1
            case splice: Splice =>
              level -= 1
              traverse(splice.expr)
              level += 1
            case _ => traverseChildren(tree)
        checker.traverse(q.body)
      case _ => ()
    ctx

  override def transformUnit(tree: Tree)(using Context): Tree = rewrite(tree, Map.empty)

  // Traverse quotes explicitly: a quote inside a splice needs the outer binder environment.
  private def rewrite(tree: Tree, env: Map[Symbol, Tree])(using Context): Tree =
    val mapper = new TreeMap:
      override def transform(tree: Tree)(using Context): Tree = tree match
        case app @ Apply(Select(q: Quote, _), _) if !q.isTypeQuote => elaborate(app, env)
        case _ => super.transform(tree)
    mapper.transform(tree)

  private def elaborate(tree: Apply, initialEnv: Map[Symbol, Tree])(using Context): Tree = tree match
    case Apply(Select(q: Quote, _), List(quotes)) if !q.isTypeQuote =>

      def call(name: String, args: List[Tree], contextual: Boolean = false): Tree =
        val base = ref(requiredModule("quotation.Combinators")).select(name.toTermName).appliedToArgs(args)
        if contextual then base.appliedTo(quotes) else base

      def leaf(body: Tree): Tree = Quote(body, q.tags).select("apply".toTermName).appliedTo(quotes).asInstance(exprAny)

      def unsupported(body: Tree, message: String): Tree =
        report.error(message, body.srcPos)
        leaf(Literal(dotty.tools.dotc.core.Constants.Constant(())))

      def elab(body: Tree, env: Map[Symbol, Tree]): Tree = body match
        case _: Quote => unsupported(body, "use-quotation supports only two stages: nested quotation is unsupported")
        // Escape back to generator code; recursively elaborate its embedded quotations.
        case s: Splice => rewrite(s.expr, env).select("apply".toTermName).appliedTo(quotes).asInstance(exprAny)
        case id: Ident => call("mkVar", List(env.getOrElse(id.symbol, leaf(id))))
        case lit: Literal => call("mkLiteral", List(leaf(lit)))
        // Typed Scala lambdas are represented by a synthetic method and a Closure.
        case Block(List(d: DefDef), _: Closure) if d.termParamss.flatten.size == 1 =>
          val parameter = d.termParamss.flatten.head
          val mt = MethodType(List("code".toTermName))(_ => List(exprAny), _ => exprAny)
          val callback = Lambda(mt, args =>
            ref(requiredModule("quotation.Combinators")).select("reify".toTermName)
              .appliedToType(defn.AnyType)
              .appliedTo(elab(d.rhs, env.updated(parameter.symbol, args.head))).appliedTo(quotes)
          )
          val tag = Quote(TypeTree(body.tpe.widen), q.tags).select("apply".toTermName).appliedTo(quotes)
          ref(requiredModule("quotation.Combinators"))
            .select("mkLam".toTermName)
            .appliedToType(body.tpe.widen)
            .appliedToArgs(List(tag, callback)).appliedTo(quotes)
        case Apply(fun, List(arg)) if
          fun.symbol.name.toString == "println" &&
          fun.symbol.owner.fullName.toString.startsWith("scala.Predef") =>
          call("mkPrint", List(elab(arg, env)), true)
        case Apply(Select(lhs, name), List(rhs)) if
          name.toString == "+" && lhs.tpe.widen =:= defn.IntType && rhs.tpe.widen =:= defn.IntType =>
          call("mkAdd", List(elab(lhs, env), elab(rhs, env)), true)
        case Apply(Select(fun, name), List(arg)) if
          name.toString == "apply" &&
          fun.tpe.widen.derivesFrom(defn.FunctionType(1).typeSymbol) =>
          call("mkApp", List(elab(fun, env), elab(arg, env)), true)
        case Block(Nil, expr) => elab(expr, env)
        case Block(stat :: rest, expr) if !stat.isInstanceOf[ValDef] && !stat.isInstanceOf[DefDef] =>
          call("mkSeq", List(elab(stat, env), elab(Block(rest, expr), env)), true)
        case Inlined(_, Nil, expr) => elab(expr, env)
        case Typed(expr, _) => elab(expr, env)
        case _ => unsupported(body, s"Unsupported quoted syntax in use-quotation: ${body.show}")

      elab(q.body, initialEnv).asInstance(tree.tpe).withSpan(tree.span)

    case _ => tree
