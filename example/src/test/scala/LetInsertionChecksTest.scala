import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import useQuotation.Runtime.{reify, run, reflect}
import useQuotation.Syntax.*
import munit.FunSuite

class LetInsertionTest extends FunSuite with Base:
  given Compiler = Compiler.make(getClass.getClassLoader)

  def plus(x: Expr[Int])(using Quotes): Expr[Int] =
    !'{ $x + $x }

  test("let-insertion internals") {
    withQuotes {
      import quotes.reflect.*
      def strip(term: Term): Term = term match
        case Inlined(_, Nil, inner) => strip(inner)
        case Typed(inner, _) => strip(inner)
        case _ => term

      val code = reify { plus(!'{ println("once"); 21 }) }
      val Block(bindings, result) = strip(code.asTerm): @unchecked
      assertEquals(bindings.size, 2)

      val List(printBinding: ValDef, addBinding: ValDef) = bindings: @unchecked
      assert(printBinding.tpt.tpe =:= TypeRepr.of[Unit])
      assert(addBinding.tpt.tpe =:= TypeRepr.of[Int])
      assert(strip(result).symbol == addBinding.symbol)

      val shared = reify { plus(!'{ ((x: Int) => x)(21) }) }
      val Block(List(fun: ValDef, app: ValDef, add: ValDef), last) = strip(shared.asTerm): @unchecked
      val Apply(Select(receiver, _), _) = strip(app.rhs.get): @unchecked
      assertEquals(strip(receiver).symbol, fun.symbol)
      val Apply(Select(lhs, _), List(rhs)) = strip(add.rhs.get): @unchecked
      assertEquals(strip(lhs).symbol, app.symbol)
      assertEquals(strip(rhs).symbol, app.symbol)
      assertEquals(strip(last).symbol, add.symbol)

      val function = reify { !'{ (x: Int) => { println(x); x + 1 } } }
      // mkLam reflects the function itself; its body has a separate reify frame.
      val Block(List(lambda: ValDef), lambdaResult) = strip(function.asTerm): @unchecked
      assertEquals(strip(lambdaResult).symbol, lambda.symbol)

      val Block(List(method: DefDef), _: Closure) = strip(lambda.rhs.get): @unchecked
      val Block(localBindings, localResult) = strip(method.rhs.get): @unchecked
      assertEquals(localBindings.size, 2)
      assert(localBindings.forall(_.symbol.owner == method.symbol))
      assertEquals(strip(localResult).symbol, localBindings.last.symbol)

      // An inner frame cannot consume bindings accumulated by its parent.
      val nested = reify {
        val x = !'{ 1 + 2 }
        val inner = reify { !'{ 3 + 4 } }
        val Block(innerBindings, _) = strip(inner.asTerm): @unchecked
        assert(innerBindings.size == 1)
        x
      }
      val Block(outerBindings, _) = strip(nested.asTerm): @unchecked
      assertEquals(outerBindings.size, 1)

      val recovered = reify {
        val x = !'{ 1 + 2 }
        try reify { !'{ println("must not escape") }; throw new RuntimeException("test") }
        catch case _: RuntimeException => ()
        x
      }
      val Block(recoveredBindings, _) = strip(recovered.asTerm): @unchecked
      assertEquals(recoveredBindings.size, 1)

      try
        reflect('{ 1 })
        assert(false, "reflect outside reify should fail")
      catch case e: IllegalStateException => assert(e.getMessage.contains("reify"))
      val Block(empty, _) = strip(reify { '{ 1 } }.asTerm): @unchecked
      assert(empty.isEmpty)
    }
  }

  test("no duplication in lambda - 1") {
    val (shared, sharedLines) = captureOut {
      run { plus(!'{ ((x: Int) => { println("call"); x })(21) }) }
    }
    assertEquals(shared, 42)
    assertEquals(sharedLines, List("call"))
  }

  test("print in lambda") {
    val (separate, separateLines) = captureOut {
      run {
        val a = !'{ ((x: Int) => { println("first"); x })(20) }
        val b = !'{ ((x: Int) => { println("second"); x })(22) }
        !'{ $a + $b }
      }
    }
    assertEquals(separate, 42)
    assertEquals(separateLines, List("first", "second"))
  }

  test("no discard") {
    val (discarded, discardedLines) = captureOut {
      run {
        val unused = !'{ println("discarded") }
        '{ 42 }
      }
    }
    assertEquals(discarded, 42)
    assertEquals(discardedLines, List("discarded"))
  }

  test("twice") {
    val (twice, lambdaLines) = captureOut {
      val f = run { !'{ (x: Int) => { println(x); x + 1 } } }
      assertEquals(f(4), 5)
      f(5)
    }
    assertEquals(twice, 6)
    assertEquals(lambdaLines, List("4", "5"))
  }

  test("exception in reify"):
    intercept[RuntimeException] {
      withQuotes { reify { !'{ println("aborted") }; throw new RuntimeException("test") } }
    }
