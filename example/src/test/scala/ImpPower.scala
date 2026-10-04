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
