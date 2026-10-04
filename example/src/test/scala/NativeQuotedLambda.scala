import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import useQuotation.Runtime.{reify, run}
import useQuotation.Syntax.*
import munit.FunSuite

class NativeLambdaTest extends FunSuite with Base:
  given Compiler = Compiler.make(getClass.getClassLoader)

  def twice(x: Expr[Int])(using Quotes): Expr[Int] = !'{ $x + $x }

  def traced(x: Expr[Int], label: String)(using Quotes): Expr[Int] =
    !'{
      println(${Expr(label)})
      $x + $x
    }

  def sharedCode(using Quotes): Expr[Int => Int] =
    '{
      (x: Int) => ${
        val shared = !'{ ((n: Int) => { println("call"); n })(x) }
        '{ $shared + $shared }
      }
    }

  test("body sharing") {
    /*
    sharedCode should be equivalent to the following:
    ((x: Int) => {
      val x6: Int => Int = ((x4: Int) => {
        val x5 = println("call")
        x4
      })
      val x7: Int = x6(x)
      x7 + x7
    })
    */
    withQuotes { println(reify { sharedCode }.show) }

    withQuotes {
      import quotes.reflect.*
      def strip(t: Term): Term = t match
        case Inlined(_, Nil, body) => strip(body)
        case Typed(body, _) => strip(body)
        case _ => t
      val Block(List(method: DefDef), _: Closure) = strip(sharedCode.asTerm): @unchecked
      val Block(List(_: ValDef, app: ValDef), sum) = strip(method.rhs.get): @unchecked
      val Apply(Select(lhs, _), List(rhs)) = strip(sum): @unchecked
      assertEquals(strip(lhs).symbol, app.symbol)
      assertEquals(strip(rhs).symbol, app.symbol)
      assertEquals(app.symbol.owner, method.symbol)
    }
    val (f, generatedOutput) = captureOut {
      // A native lambda supplies/reifies its own body frame.
      staging.run { sharedCode }
    }
    assert(generatedOutput.isEmpty)
    val (result, lines) = captureOut { (f(20), f(21)) }
    assertEquals(result, (40, 42))
    assertEquals(lines, List("call", "call"))
  }

  test("splice order") {
    withQuotes { println(reify { '{ (x: Int) => {
        ${traced('x, "first")}
        ${traced('x, "second")}
      } } }.show) }
    /*
    Generates code:
      ((x: Int) => {
        val x0: Unit = println("first")
        val x1: Int = x + x
        val x2: Unit = println("second")
        val x3: Int = x + x
        x1
        x3
      })
    */

    val f = run {
      '{ (x: Int) => {
        ${traced('x, "first")}
        ${traced('x, "second")}
      } }
    }
    val (result, lines) = captureOut { (f(20), f(21)) }
    assertEquals(result, (40, 42))
    assertEquals(lines, List("first", "second", "first", "second"))
  }

  test("nested scopes") {
    val nested = run { '{ (x: Int) => (y: Int) => ${twice('{ x + y })} } }
    assertEquals(nested(20)(1), 42)

    val shadowed = run { '{ (x: Int) => ((x: Int) => ${twice('x)})(21) } }
    assertEquals(shadowed(99), 42)

    val captured = run {
      val outer = !'{ 20 + 1 }
      '{ (x: Int) => ${twice('{ $outer + x })} }
    }
    assertEquals(captured(0), 42)

    withQuotes {
      import quotes.reflect.*
      val code = reify {
        val outer = !'{ 20 + 1 }
        intercept[RuntimeException] {
          '{ (x: Int) => ${throw new RuntimeException("body failed")} }
        }
        !'{ $outer + $outer }
      }
      def strip(t: Term): Term = t match
        case Inlined(_, Nil, body) => strip(body)
        case _ => t
      val Block(bindings, result) = strip(code.asTerm): @unchecked
      assertEquals(bindings.size, 2)
      assertEquals(strip(result).symbol, bindings.last.symbol)
    }
  }

  test("no duplication") {
    val noDuplication = run {
      '{ (x: Int) => ${
        val code = !'{ println("native"); x }
        '{ $code + $code }
      } }
    }
    val (value, lines) = captureOut { noDuplication(21) }
    assertEquals(value, 42)
    assertEquals(lines, List("native"))
  }

  test("mention-quotation behavior preserves") {
    val f = staging.run {
      '{ (x: Int) => {
        val n = x + 1
        if n > 2 then List(n, n).sum else 0
      } }
    }
    assertEquals(f(3), 8)
    assertEquals(f(0), 0)

    // Scope instrumentation also survives native quotation at later stages.
    val nextStage = staging.run {
      '{ (q: Quotes) =>
        given Quotes = q
        '{ (x: Int) => ${useQuotation.Runtime.reflect('x)} }
      }
    }
    val identity = scala.quoted.staging.run { nextStage(summon[Quotes]) }
    assertEquals(identity(42), 42)
  }
