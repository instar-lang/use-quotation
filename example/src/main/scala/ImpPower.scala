import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import quotation.Runtime.{reify, run}

object ImpPower:

  given Compiler = Compiler.make(getClass.getClassLoader)
  case class Mut[T](var x: T)

  def impPower(x: Expr[Double], n: Int, result: Expr[Mut[Double]])(using Quotes): Expr[Double] =
    if n == 0 then '{ ${result}.x }
    else '{
      ${result}.x = $x * ${result}.x;
      ${impPower(x, n - 1, result)}
    }

  def powerCode(n: Int)(using Quotes): Expr[Double => Double] =
    val res = '{ Mut[Double](1.0) }
    '{ (x: Double) => ${impPower('x, n, res)} }

  def specImpPower(n: Int): Double => Double = run { powerCode(n) }

  def main(args: Array[String]): Unit =
    withQuotes { println(reify { powerCode(3) }.show) }
    val cube = specImpPower(3)
    assert(cube(4) == 4 * 4 * 4)
    // Note that cube is a function that captures a mutable cell,
    // so it is not referentially transparent.
    assert(specImpPower(3)(2.0) == 8.0)
    assert(specImpPower(3)(3.0) == 27.0)
    assert(specImpPower(0)(10.0) == 1.0)
