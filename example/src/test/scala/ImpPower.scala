import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import useQuotation.Runtime.{reify, run}
import useQuotation.Syntax.*

import munit.FunSuite

object ImpPowerTest:
  case class Mut[T](var x: T)

class ImpPowerTest extends FunSuite with Base:
  import ImpPowerTest.Mut

  given Compiler = Compiler.make(getClass.getClassLoader)

  {
    // Note that in impPower and powerCode, we use use-quotation everywhere.
    // The real crucial one is the initial allocation of the mutable cell.
    def impPower(x: Expr[Double], n: Int, result: Expr[Mut[Double]])(using Quotes): Expr[Double] =
      if n == 0 then !'{ ${result}.x }
      else !'{
        ${result}.x = $x * ${result}.x;
        ${impPower(x, n - 1, result)}
      }

    def powerCode(n: Int)(using Quotes): Expr[Double => Double] =
      val res = !'{ Mut[Double](1.0) }
      !'{ (x: Double) => ${impPower('x, n, res)} }

    def specImpPower(n: Int): Double => Double = run { powerCode(n) }

    test("power3") {
      withQuotes { println(reify { powerCode(3) }.show) }
    }

    test("imperative power code gen") {
      val cube = specImpPower(3)
      assertEquals(cube(4), 64.0)

      // Note that cube is a function that captures a mutable cell,
      // so it is not referentially transparent.
      assertEquals(specImpPower(3)(2.0), 8.0)
      assertEquals(specImpPower(3)(3.0), 27.0)
      assertEquals(specImpPower(0)(10.0), 1.0)
    }
  }

  {
    // Test cases examine mixing use of mention-quotation and use-quotation.
    // Note that we use use-quotation in the function body of impPower,
    // but they are not in a use-quotation context:
    // powerCode calls impPower with a mention-quotation context.
    // These code should be generated in the quoted lambda returned by powerCode.
    def impPower(x: Expr[Double], n: Int, result: Expr[Mut[Double]])(using Quotes): Expr[Double] =
      if n == 0 then '{ ${result}.x }
      else !'{
        ${result}.x = $x * ${result}.x;
        ${impPower(x, n - 1, result)}
      }

    def powerCode(n: Int)(using Quotes): Expr[Double => Double] =
      val res = !'{ Mut[Double](1.0) }
      // To ensure the generated code is correct, mention-quoted lambda
      // also implicitly creates a reify-context.
      '{ (x: Double) => ${impPower('x, n, res)} }

    def specImpPower(n: Int): Double => Double = run { powerCode(n) }

    test("imperative power - mix mention- and use-quotation".only) {
      withQuotes { println(reify { powerCode(3) }.show) }
      val cube = specImpPower(3)
      assertEquals(cube(4), 64.0)
      // Note that cube is a function that captures a mutable cell,
      // so it is not referentially transparent.
      assertEquals(specImpPower(3)(2.0), 8.0)
      assertEquals(specImpPower(3)(3.0), 27.0)
      assertEquals(specImpPower(3)(4.0), 64.0)
      assertEquals(specImpPower(0)(10.0), 1.0)
    }

    test("power generated code internals") {
      withQuotes {
        import quotes.reflect.*
        def strip(t: Term): Term = t match
          case Inlined(_, Nil, body) => strip(body)
          case Typed(body, _) => strip(body)
          case _ => t

        val Block(List(cell: ValDef), lambda) = strip(reify { powerCode(2) }.asTerm): @unchecked
        assert(cell.tpt.tpe =:= TypeRepr.of[Mut[Double]])
        val Block(List(method: DefDef), _: Closure) = strip(lambda): @unchecked
        val Block(bindings, result) = strip(method.rhs.get): @unchecked
        assertEquals(bindings.map(_.asInstanceOf[ValDef].tpt.tpe),
          List(TypeRepr.of[Double], TypeRepr.of[Double], TypeRepr.of[Unit],
            TypeRepr.of[Double], TypeRepr.of[Double], TypeRepr.of[Unit]))
        assert(bindings.forall(_.symbol.owner == method.symbol))
        val Select(receiver, "x") = strip(result): @unchecked
        assertEquals(strip(receiver).symbol, cell.symbol)
      }
    }

  }