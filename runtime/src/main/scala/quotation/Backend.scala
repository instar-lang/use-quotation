package useQuotation
import scala.quoted.*
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.ListBuffer

/** Stateful let insertion; code values share bindings until the enclosing reify. */
object Runtime:
  private val names = new AtomicLong
  private def freshName = s"x${names.getAndIncrement()}"
  private class Frame(val quotes: Quotes):
    import quotes.reflect.*
    private val owner = Symbol.spliceOwner
    private val bindings = ListBuffer.empty[ValDef]

    def reflect[T](rhs: Expr[T]): Expr[T] =
      val term = rhs.asTerm
      val symbol = Symbol.newVal(owner, freshName, term.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
      bindings += ValDef(symbol, Some(term.changeOwner(symbol)))
      Ref(symbol).asExpr.asInstanceOf[Expr[T]]

    def finish[T](result: Expr[T]): Expr[T] =
      Block(bindings.toList, result.asTerm).asExpr.asInstanceOf[Expr[T]]

  private val active = new ThreadLocal[Frame]

  def reify[T](body: => Expr[T])(using Quotes): Expr[T] =
    val previous = active.get()
    val frame = new Frame(summon[Quotes])
    active.set(frame)
    try frame.finish(body)
    finally
      if previous == null then active.remove() else active.set(previous)

  def reflect[T](rhs: Expr[T])(using Quotes): Expr[T] =
    val frame = active.get()
    if frame == null then
      throw new IllegalStateException("Let insertion requires Runtime.run { ... } or Runtime.reify { ... } around the whole generator")
    frame.reflect(rhs)

  /** Compile and execute code with one reification boundary for the whole generator. */
  def run[T](body: Quotes ?=> Expr[T])(using staging.Compiler): T =
    staging.run { reify { body(using summon[Quotes]) } }

  def mkVar(value: Expr[Any]): Expr[Any] = value
  def mkLiteral(value: Expr[Any]): Expr[Any] = value
  def mkApp(fun: Expr[Any], arg: Expr[Any])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    reflect(Apply(Select.unique(fun.asTerm, "apply"), List(arg.asTerm)).asExpr)
  def mkPrint(arg: Expr[Any])(using Quotes): Expr[Any] =
    reflect('{ Predef.println($arg) })
  def mkAdd(lhs: Expr[Any], rhs: Expr[Any])(using Quotes): Expr[Any] =
    reflect('{ ${lhs.asExprOf[Int]} + ${rhs.asExprOf[Int]} })
  def mkMul(lhs: Expr[Any], rhs: Expr[Any])(using Quotes): Expr[Any] =
    reflect('{ ${lhs.asExprOf[Double]} * ${rhs.asExprOf[Double]} })

  /** Read at this point in the generated program, even if the member later changes. */
  def mkSelect(receiver: Expr[Any], name: String)(using Quotes): Expr[Any] =
    import quotes.reflect.*
    reflect(Select.unique(receiver.asTerm, name).asExpr)

  def mkAssign(receiver: Expr[Any], name: String, value: Expr[Any])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    reflect(Assign(Select.unique(receiver.asTerm, name), value.asTerm).asExpr)

  /** Ordinary strict method calls, including generic companion factory calls. */
  def mkCall(receiver: Expr[Any], name: String, types: List[Type[?]], args: List[Expr[Any]])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val typeArgs = types.map(t => TypeRepr.of(using t))
    reflect(Select.overloaded(receiver.asTerm, name, typeArgs, args.map(_.asTerm)).asExpr)

  def mkSeq(first: Expr[Any], rest: Expr[Any])(using Quotes): Expr[Any] =
    // Both arguments have already been constructed in left-to-right order.
    rest

  // Higher-order abstract syntax: the callback receives code for the fresh parameter.
  def mkLam[T](tpe: Type[T], body: Expr[Any] => Expr[Any])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val AppliedType(_, List(in, out)) = TypeRepr.of[T](using tpe): @unchecked
    val methodType = MethodType(List(freshName))(_ => List(in), _ => out)
    val fun = Lambda(Symbol.spliceOwner, methodType,
      (owner, args) =>
        val param = args.head.asInstanceOf[Term].asExpr
        body(param).asTerm.changeOwner(owner)
    ).asExpr
    reflect(fun)
