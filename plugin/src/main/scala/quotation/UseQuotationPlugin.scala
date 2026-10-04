package quotation
import dotty.tools.dotc.ast.tpd

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Constants.Constant
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

  // Native quotations keep Scala's staging rules; this restriction is opt-in.
  private def checkTwoStages(q: Quote)(using Context): Unit =
    val checker = new TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit = tree match
        case quote: Quote if !quote.isTypeQuote =>
          report.error("use-quotation supports only two stages: nested quotation is unsupported", quote.srcPos)
        case _: Quote => ()
        // Generator code is inspected separately by rewrite. Its native quotes
        // may use all of Scala's stages; marked quotes get their own check.
        case _: Splice => ()
        case _ => traverseChildren(tree)
    checker.traverse(q.body)

  override def transformUnit(tree: Tree)(using Context): Tree =
    // Body scopes introduce quote/splice pairs after staging. Recollect witnesses
    // at their new boundaries before splicing lifts and pickles the generators.
    new dotty.tools.dotc.staging.CrossStageSafety().transform(rewrite(tree, Map.empty))

  private def directQuote(tree: Tree): Option[Apply] = tree match
    case app @ Apply(Select(q: Quote, _), List(_)) if !q.isTypeQuote => Some(app)
    case Typed(expr, _) => directQuote(expr)
    case Inlined(_, Nil, expr) => directQuote(expr)
    case Block(Nil, expr) => directQuote(expr)
    case _ => None

  private def rewrite(tree: Tree, env: Map[Symbol, Tree])(using Context): Tree =
    val mapper = new TreeMap:
      private var level = 0
      private var inNativeQuote = false
      override def transform(tree: Tree)(using Context): Tree = tree match
        case app @ Apply(fun, List(arg)) if fun.symbol == requiredMethod("useQuotation.Syntax.unary_!") =>
          if level != 0 then
            report.error("Use-quotation is supported only at the generator stage", app.srcPos)
            arg
          else directQuote(arg) match
            case Some(quote @ Apply(Select(q: Quote, _), _)) =>
              checkTwoStages(q)
              elaborate(quote, env).asInstance(app.tpe).withSpan(app.span)
            case None =>
              // Scala can cancel !'{ $code } to !code before this phase.
              // Already constructed code is passed through, never re-elaborated.
              transform(arg).withSpan(app.span)
            case _ => tree // directQuote only returns quotation applications.
        case q: Quote if !q.isTypeQuote =>
          val previous = inNativeQuote
          inNativeQuote = true
          level += 1
          try super.transform(q)
          finally
            level -= 1
            inNativeQuote = previous
        case block @ Block(List(_: DefDef), _: Closure) if level > 0 && inNativeQuote =>
          // Native syntax stays native, but each quoted lambda body gets a reification frame.
          val transformed = super.transform(block).asInstanceOf[Block]
          val method = transformed.stats.head.asInstanceOf[DefDef]
          val resultType = method.rhs.tpe.widen
          val mt = ContextualMethodType(List("quotes".toTermName))(
            _ => List(requiredClass("scala.quoted.Quotes").typeRef), _ => exprAny
          )
          val fun = Lambda(mt, args =>
            // CrossStageSafety recollects the witnesses for this new native quote.
            val body = Quote(method.rhs, Nil).select("apply".toTermName)
              .appliedTo(args.head).asInstance(exprAny)
            ref(requiredModule("useQuotation.Runtime")).select("reify".toTermName)
              .appliedToType(defn.AnyType).appliedTo(body).appliedTo(args.head)
          )(using ctx.withOwner(method.symbol))
          val scopedBody = Splice(fun, resultType).withSpan(method.rhs.span)
          cpy.Block(transformed)(
            stats = List(cpy.DefDef(method)(rhs = scopedBody)), expr = transformed.expr
          )
        case s: Splice =>
          val previous = inNativeQuote
          inNativeQuote = false
          level -= 1
          try super.transform(s)
          finally
            level += 1
            inNativeQuote = previous
        // A native quote in a splice may mention a parameter replaced by mkLam.
        // Insert the callback's code value using a native splice, not via our backend combinators.
        case id: Ident if level == 1 && env.contains(id.symbol) =>
          val codeType = defn.QuotedExprClass.typeRef.appliedTo(id.tpe.widen)
          val mt = ContextualMethodType(List("quotes".toTermName))(
            _ => List(requiredClass("scala.quoted.Quotes").typeRef), _ => codeType
          )
          Splice(Lambda(mt, _ => env(id.symbol).asInstance(codeType)), id.tpe.widen).withSpan(id.span)
        case _ => super.transform(tree)
    mapper.transform(tree)

  private def elaborate(tree: Apply, initialEnv: Map[Symbol, Tree])(using Context): Tree = tree match
    case Apply(Select(q: Quote, _), List(quotes)) if !q.isTypeQuote =>

      def call(name: String, args: List[Tree], contextual: Boolean = false): Tree =
        val base = ref(requiredModule("useQuotation.Runtime")).select(name.toTermName).appliedToArgs(args)
        if contextual then base.appliedTo(quotes) else base

      def leaf(body: Tree): Tree = Quote(body, q.tags).select("apply".toTermName).appliedTo(quotes).asInstance(exprAny)

      def typeTag(tpe: Type): Tree =
        tpe match
          // Staging already healed abstract T to witness.Underlying. Reuse its witness.
          case tp @ TypeRef(tag: TermRef, _) if tp.typeSymbol == defn.QuotedType_splice => ref(tag)
          case _ =>
            // This type quotation is introduced after staging. Collect its own witnesses,
            // including abstract types nested inside applied types such as Option[T].
            val tag = Quote(TypeTree(tpe), Nil).select("apply".toTermName).appliedTo(quotes)
            new dotty.tools.dotc.staging.CrossStageSafety().transform(tag)

      def methodCall(fun: Tree, receiver: Tree, name: String, types: List[Tree], args: List[Tree], env: Map[Symbol, Tree]): Tree =
        fun.tpe.widen match
          case mt: MethodType if !mt.isImplicitMethod &&
              !mt.paramInfos.exists(t => t.isInstanceOf[ExprType] || t.isRepeatedParam) &&
              !mt.resType.isInstanceOf[MethodType] && !mt.resType.isInstanceOf[PolyType] =>
            call("mkCall", List(
              elab(receiver, env), Literal(Constant(name)),
              mkList(types.map(t => typeTag(t.tpe)), TypeTree(defn.QuotedTypeClass.typeRef.appliedTo(TypeBounds.empty))),
              mkList(args.map(elab(_, env)), TypeTree(exprAny))
            ), true)
          case _ => unsupported(fun, "use-quotation supports only strict method calls with one ordinary argument list")

      def unsupported(body: Tree, message: String): Tree =
        report.error(message, body.srcPos)
        leaf(Literal(dotty.tools.dotc.core.Constants.Constant(())))

      def elab(body: Tree, env: Map[Symbol, Tree]): Tree = body match
        case _: Quote => unsupported(body, "use-quotation supports only two stages: nested quotation is unsupported")
        // Escape back to generator code; recursively elaborate its embedded quotations.
        case s: Splice => rewrite(s.expr, env).select("apply".toTermName).appliedTo(quotes).asInstance(exprAny)
        case id: Ident => call("mkVar", List(env.getOrElse(id.symbol, leaf(id))))
        // A statically reachable module is an atomic receiver, not an effectful member read.
        case sel: Select if sel.symbol.is(Module) && sel.symbol.isStatic => call("mkVar", List(leaf(sel)))
        case lit: Literal => call("mkLiteral", List(leaf(lit)))
        // Typed Scala lambdas are represented by a synthetic method and a Closure.
        case Block(List(d: DefDef), _: Closure) if d.termParamss.flatten.size == 1 =>
          val parameter = d.termParamss.flatten.head
          val mt = MethodType(List("code".toTermName))(_ => List(exprAny), _ => exprAny)
          val callback = Lambda(mt, args =>
            ref(requiredModule("useQuotation.Runtime")).select("reify".toTermName)
              .appliedToType(defn.AnyType)
              .appliedTo(elab(d.rhs, env.updated(parameter.symbol, args.head))).appliedTo(quotes)
          )
          val tag = typeTag(body.tpe.widen)
          ref(requiredModule("useQuotation.Runtime"))
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
        case Apply(Select(lhs, name), List(rhs)) if
          name.toString == "*" && lhs.tpe.widen =:= defn.DoubleType && rhs.tpe.widen =:= defn.DoubleType =>
          call("mkMul", List(elab(lhs, env), elab(rhs, env)), true)
        case Apply(Select(fun, name), List(arg)) if
          name.toString == "apply" &&
          fun.tpe.widen.derivesFrom(defn.FunctionType(1).typeSymbol) =>
          call("mkApp", List(elab(fun, env), elab(arg, env)), true)
        case Apply(fun @ TypeApply(Select(receiver, name), types), args) =>
          methodCall(fun, receiver, name.toString, types, args, env)
        case Apply(fun @ Select(receiver, name), args) =>
          methodCall(fun, receiver, name.toString, Nil, args, env)
        case Assign(Select(receiver, name), rhs) =>
          call("mkAssign", List(elab(receiver, env), Literal(Constant(name.toString)), elab(rhs, env)), true)
        case sel: Select if !sel.tpe.widen.isInstanceOf[MethodType] && !sel.tpe.widen.isInstanceOf[PolyType] =>
          call("mkSelect", List(elab(sel.qualifier, env), Literal(Constant(sel.name.toString))), true)
        case Block(Nil, expr) => elab(expr, env)
        case Block(stat :: rest, expr) if !stat.isInstanceOf[ValDef] && !stat.isInstanceOf[DefDef] =>
          call("mkSeq", List(elab(stat, env), elab(Block(rest, expr), env)), true)
        case Inlined(_, Nil, expr) => elab(expr, env)
        case Typed(expr, _) => elab(expr, env)
        case _ => unsupported(body, s"Unsupported quoted syntax in use-quotation: ${body.show}")

      elab(q.body, initialEnv).asInstance(tree.tpe).withSpan(tree.span)

    case _ => tree
