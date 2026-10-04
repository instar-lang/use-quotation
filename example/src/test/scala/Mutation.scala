import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import useQuotation.Runtime.{reify, run}
import useQuotation.Syntax.*
import munit.FunSuite

// Generated code refers to these helpers through a stable companion path.
object MutationChecksTest:
  case class Mut[T](var x: T)
  private var allocations = 0

  def makeCell[T](value: T): Mut[T] =
    allocations += 1
    Mut(value)

  def genericCell[T: Type](value: Expr[T])(using Quotes): Expr[Mut[T]] =
    !'{ Mut[T]($value) }

  def genericOptionCell[T: Type](value: Expr[T])(using Quotes): Expr[Mut[Option[T]]] =
    !'{ Mut[Option[T]](Option.apply[T]($value)) }

  def impPower(x: Expr[Double], n: Int, result: Expr[Mut[Double]])(using Quotes): Expr[Double] =
    if n == 0 then !'{ ${result}.x }
    else !'{
      ${result}.x = $x * ${result}.x;
      ${impPower(x, n - 1, result)}
    }

  def powerCode(n: Int)(using Quotes): Expr[Double => Double] =
    val res = !'{ Mut[Double](1.0) }
    !'{ (x: Double) => ${impPower('x, n, res)} }

class MutationChecksTest extends FunSuite with Base:
  import MutationChecksTest.*

  given Compiler = Compiler.make(getClass.getClassLoader)

  test("power trees") {
    withQuotes {
      import quotes.reflect.*
      def strip(t: Term): Term = t match
        case Inlined(_, Nil, e) => strip(e)
        case Typed(e, _) => strip(e)
        case _ => t

      val code = reify { powerCode(2) }
      val Block(List(allocation: ValDef, function: ValDef), result) = strip(code.asTerm): @unchecked
      assert(allocation.tpt.tpe =:= TypeRepr.of[Mut[Double]])
      assert(function.tpt.tpe =:= TypeRepr.of[Double => Double])
      assertEquals(strip(result).symbol, function.symbol)
      val Block(List(method: DefDef), _: Closure) = strip(function.rhs.get): @unchecked
      val Block(bindings, last) = strip(method.rhs.get): @unchecked
      val actualTypes = bindings.map(_.asInstanceOf[ValDef].tpt.tpe)
      val expectedTypes = List(TypeRepr.of[Double], TypeRepr.of[Double], TypeRepr.of[Unit],
        TypeRepr.of[Double], TypeRepr.of[Double], TypeRepr.of[Unit], TypeRepr.of[Double])
      assertEquals(actualTypes.size, expectedTypes.size)
      // Equivalent types may have different internal prefixes after unpickling.
      for ((actual, expected), index) <- actualTypes.zip(expectedTypes).zipWithIndex do
        assert(actual =:= expected, s"binding $index: ${actual.show} != ${expected.show}")
      assert(bindings.forall(_.symbol.owner == method.symbol))
      assertEquals(strip(last).symbol, bindings.last.symbol)
    }
  }

  test("mutation effects") {
    // Reads snapshot the member before later mutations, including discarded writes.
    val snapshot = run {
      val cell = !'{ Mut[Double](2.0) }
      val before = !'{ $cell.x }
      val write = !'{ $cell.x = 9.0 }
      val after = !'{ $cell.x }
      !'{ $before * $after }
    }
    assertEquals(snapshot, 18.0)

    // Reusing allocation code reuses one object rather than allocating per splice.
    assertEquals(run {
      val cell = !'{ Mut[Double](1.0) }
      val write = !'{ $cell.x = 7.0 }
      !'{ $cell.x }
    }, 7.0)

    val (value, order) = captureOut {
      run {
        val cell = !'{ Mut[Double](1.0) }
        val rhs = !'{ println("write"); 4.0 }
        val changed = !'{ $cell.x = $rhs }
        !'{ println("read"); $cell.x }
      }
    }
    assertEquals(value, 4.0)
    assertEquals(order, List("write", "read"))
  }

  test("method calls") {
    // Generic and overloaded strict methods use the actual argument types.
    assertEquals(run { !'{ Option.apply[Int](3).get } }, 3)
    assertEquals(run { !'{ java.lang.Math.max(2.0, 3.0) } }, 3.0)
    assertEquals(run { val cell = genericCell(!'{ 5 }); !'{ $cell.x } }, 5)
    assertEquals(run { val cell = genericOptionCell(!'{ 5 }); !'{ $cell.x.get } }, 5)
  }

  test("allocation scope") {
    allocations = 0
    assertEquals(run {
      val cell = !'{ MutationChecksTest.makeCell[Double](1.0) }
      val write = !'{ $cell.x = 6.0 }
      !'{ $cell.x * $cell.x }
    }, 36.0)
    assertEquals(allocations, 1)
    assertEquals(run { !'{ ((x: Double) => x * x)(3.0) } }, 9.0)

    // Generating the allocation inside the lambda's splice makes it local.
    val fresh = run { !'{ (x: Double) => ${
      val cell = !'{ Mut[Double](1.0) }
      impPower('x, 3, cell)
    } } }
    assertEquals(fresh(2.0), 8.0)
    assertEquals(fresh(3.0), 27.0)
  }
