package quotation
import scala.quoted.*

/** Stateful let insertion; code values share bindings until the enclosing reify. */
object Combinators:
  private val names = new java.util.concurrent.atomic.AtomicLong
  private def freshName = s"x${names.getAndIncrement()}"
  private class Frame(val quotes: Quotes):
    import quotes.reflect.*
    private val owner = Symbol.spliceOwner
    private val bindings = scala.collection.mutable.ListBuffer.empty[ValDef]

    def reflect[T](rhs: Expr[T]): Expr[T] =
      val term = rhs.asTerm
      val symbol = Symbol.newVal(owner, freshName, term.tpe.widen,
        Flags.EmptyFlags, Symbol.noSymbol)
      bindings += ValDef(symbol, Some(term.changeOwner(symbol)))
      Ref(symbol).asExpr.asInstanceOf[Expr[T]]

    def finish[T](result: Expr[T]): Expr[T] =
      Block(bindings.toList, result.asTerm).asExpr.asInstanceOf[Expr[T]]

  private val active = new ThreadLocal[Frame]

  /** Compile and execute code with one reification boundary for the whole generator. */
  def run[T](body: Quotes ?=> Expr[T])(using scala.quoted.staging.Compiler): T =
    scala.quoted.staging.run {
      reify { body(using summon[Quotes]) }
    }

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
      throw new IllegalStateException("Let insertion requires Combinators.run { ... } or Combinators.reify { ... } around the whole generator")
    frame.reflect(rhs)

  def mkVar(value: Expr[Any]): Expr[Any] = value
  def mkLiteral(value: Expr[Any]): Expr[Any] = value
  def mkApp(fun: Expr[Any], arg: Expr[Any])(using Quotes): Expr[Any] =
    //println("mkApp")
    import quotes.reflect.*
    reflect(Apply(Select.unique(fun.asTerm, "apply"), List(arg.asTerm)).asExpr)
  def mkPrint(arg: Expr[Any])(using Quotes): Expr[Any] =
    //println("mkPrint")
    reflect('{ Predef.println($arg) })
  def mkAdd(lhs: Expr[Any], rhs: Expr[Any])(using Quotes): Expr[Any] =
    //println("mkAdd")
    reflect('{ ${lhs.asExprOf[Int]} + ${rhs.asExprOf[Int]} })
  def mkSeq(first: Expr[Any], rest: Expr[Any])(using Quotes): Expr[Any] =
    // Both arguments have already been constructed in left-to-right order.
    rest
  // Higher-order abstract syntax: the callback receives code for the fresh parameter.
  def mkLam[T](tpe: Type[T], body: Expr[Any] => Expr[Any])(using Quotes): Expr[Any] =
    //println("mkLam")
    import quotes.reflect.*
    val AppliedType(_, List(in, out)) = TypeRepr.of[T](using tpe): @unchecked
    val methodType = MethodType(List(freshName))(_ => List(in), _ => out)
    Lambda(Symbol.spliceOwner, methodType,
      (owner, args) =>
        val param = args.head.asInstanceOf[Term].asExpr
        body(param).asTerm.changeOwner(owner)
    ).asExpr
