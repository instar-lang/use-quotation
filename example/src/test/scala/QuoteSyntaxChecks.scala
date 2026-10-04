import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import useQuotation.Syntax.*
import useQuotation.Runtime.{reify, run}
import munit.FunSuite

class QuoteSyntaxChecksTest extends FunSuite with Base:
  object Other:
    extension [T](code: Expr[T])
      def unary_! : Expr[T] = code

  given Compiler = Compiler.make(getClass.getClassLoader)

  def twice(value: Expr[Int])(using Quotes): Expr[Int] = !'{ $value + $value }
  def alias(value: Expr[Int])(using Quotes): Expr[Int] = !'{ $value }

  def mixedIdentity[T: Type](using Quotes): Expr[T => T] =
    !'{ (x: T) => ${
      val code = 'x
      code
    } }

  // Native code may use syntax rejected by the alternative elaborator.
  def native(value: Expr[Int])(using Quotes): Expr[Int] =
    '{ val n = $value; if n > 0 then n else 0 }

  def nativeNested(using Quotes): Expr[Quotes ?=> Expr[Int]] = '{ '{ 1 } }

  test("quotation trees") {
    withQuotes {
      import quotes.reflect.*
      def strip(t: Term): Term = t match
        case Inlined(_, Nil, inner) => strip(inner)
        case Typed(inner, _) => strip(inner)
        case _ => t

      val Block(nativeBindings, _) = strip(reify { '{ 1 + 2 } }.asTerm): @unchecked
      assert(nativeBindings.isEmpty)
      val Block(List(sum: ValDef), result) = strip(reify { !'{ 1 + 2 } }.asTerm): @unchecked
      assert(sum.tpt.tpe =:= TypeRepr.of[Int])
      assertEquals(strip(result).symbol, sum.symbol)
    }
  }

  test("frame scopes") {
    withQuotes {
      import quotes.reflect.*
      def strip(t: Term): Term = t match
        case Inlined(_, Nil, inner) => strip(inner)
        case Typed(inner, _) => strip(inner)
        case _ => t

      val nested = reify {
        val outer = !'{ 1 + 2 }
        val inner = reify { !'{ 3 + 4 } }
        val Block(innerBindings, _) = strip(inner.asTerm): @unchecked
        assertEquals(innerBindings.size, 1)
        outer
      }
      val Block(outerBindings, _) = strip(nested.asTerm): @unchecked
      assertEquals(outerBindings.size, 1)

      val recovered = reify {
        val outer = !'{ 1 + 2 }
        intercept[RuntimeException] {
          reify { !'{ println("discarded after failure") }; throw new RuntimeException("test") }
        }
        outer
      }
      val Block(recoveredBindings, _) = strip(recovered.asTerm): @unchecked
      assertEquals(recoveredBindings.size, 1)
    }

    val error = intercept[IllegalStateException] {
      withQuotes { !'{ 1 + 2 } }
    }
    assert(error.getMessage.contains("reify"))
    for _ <- 1 to 2 do assertEquals(run { !'{ 20 + 22 } }, 42)
  }

  test("sharing") {
    val (shared, sharedOutput) = captureOut {
      run { val code = !'{ println("alternative"); 21 }; twice(code) }
    }
    assertEquals(shared, 42)
    assertEquals(sharedOutput, List("alternative"))

    val (duplicated, nativeOutput) = captureOut {
      run { val code = '{ println("native"); 21 }; twice(code) }
    }
    assertEquals(duplicated, 42)
    assertEquals(nativeOutput, List("native", "native"))

    val (value, discardedOutput) = captureOut {
      run { val unused = !'{ println("retained") }; '{ 42 } }
    }
    assertEquals(value, 42)
    assertEquals(discardedOutput, List("retained"))
  }

  test("native syntax") {
    withQuotes { assert(nativeNested.show.nonEmpty) }
    assertEquals(scala.quoted.staging.run { native('{ 42 }) }, 42)
    assertEquals(scala.quoted.staging.run { '{ scala.util.Try(42).get } }, 42)
    assertEquals(scala.quoted.staging.run { '{ List(1, 2).size } }, 2)
    assert(!false)
    assertEquals(scala.quoted.staging.run {
      Other.unary_!('{ List(1, 2).size })
    }, 2)
    assertEquals(scala.quoted.staging.run {
      val code = '{ List(1, 2).size }
      !code
    }, 2)
  }

  test("mixed lambdas") {
    val named = run { !'{ (x: Int) => ${twice('x)} } }
    assertEquals(named(5), 10)
    val nativeBody = run {
      !'{ (x: Int) => ${
        val code = '{ val n = x + 1; if n > 2 then n else 0 }
        code
      } }
    }
    assertEquals(nativeBody(3), 4)
    assertEquals(nativeBody(0), 0)
    val shadowed = run { !'{ (x: Int) => ${
      val code = '{ ((x: Int) => x + 1)(3) + x }
      code
    } } }
    assertEquals(shadowed(10), 14)
    val captured = run {
      val outer = !'{ 20 + 1 }
      !'{ (x: Int) => ${
        val code = '{ $outer + x }
        code
      } }
    }
    assertEquals(captured(21), 42)
    val nestedLambdas = run { !'{ (x: Int) => (y: Int) => ${
      val code = '{ x + y }
      code
    } } }
    assertEquals(nestedLambdas(20)(22), 42)
    assertEquals(run { mixedIdentity[Int] }(42), 42)
  }

  test("splices") {
    // Marked generator code can occur inside a native quotation's splice.
    assertEquals(run { '{ ${!'{ 20 + 22 }} } }, 42)
    assertEquals(run { alias(!'{ 20 + 22 }) }, 42)
    // A generic marked factory must retain its Type witness through pickling.
    assertEquals(run {
      val cell = MutationChecksTest.genericCell('{ 5 })
      !'{ $cell.x }
    }, 5)
    val nativeStagesInGenerator = run { !'{ (x: Int) => ${
      val stages: Expr[Quotes ?=> Expr[Int]] = '{ '{ 1 } }
      native('x)
    } } }
    assertEquals(nativeStagesInGenerator(42), 42)
  }

