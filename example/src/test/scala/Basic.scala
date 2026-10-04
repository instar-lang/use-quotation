import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import useQuotation.Runtime.{reify, run}
import useQuotation.Syntax.*

import munit.FunSuite

class BasicTests extends FunSuite with Base:
  given Compiler = Compiler.make(getClass.getClassLoader)

  def id(x: Expr[Int])(using Quotes): Expr[Int] = x

  def plus(x: Expr[Int])(using Quotes): Expr[Int] =
    !'{ $x + $x }

  def triple(x: Expr[Int])(using Quotes): Expr[Int] =
    !'{ $x + $x + $x }

  def printPlus(x: Expr[Int])(using Quotes): Expr[Int] =
    val code = !'{ println("This should not be discarded") }
    !'{ $x + $x }

  def test2(using Quotes) =
    val value = !'{ 7 }
    val spliced = !'{ ((x: Int) => x)($value) }
    spliced

  test("id"):
    val answer = run { !'{ ((x: Int) => x)(42) } }
    assertEquals(answer, 42)

  test("plus"):
    val captured = run { !'{ (x: Int) => ${ plus('x) } } }
    assertEquals(captured(5), 10)

  test("shadowed"):
    val shadowed = run { !'{ (x: Int) => ((x: Int) => x + 1)(3) } }
    assertEquals(shadowed(99), 4)

  test("splice"):
    val outer = run {
      val x = !'{ 20 + 1 }
      !'{ (y: Int) => $x + y }
    }
    assertEquals(outer(21), 42)

  test("splice no duplication"):
    val (res, out) = captureOut {
      run {
        val x = !'{ println("no dup"); 20 + 1 }
        !'{ (y: Int) => $x + $x + y }
      }
    }
    assertEquals(res(21), 21 + 21 + 21)
    assertEquals(out, List("no dup"))

  test("staging.run works with reify"):
    // Explicit reify remains composable with the wrapper and with native staging.run.
    assert(run { reify { !'{ 20 + 22 } } } == 42)
    assert(staging.run { reify { !'{ 20 + 22 } } } == 42)

  test("playground") {
    withQuotes {
      println(reify { !'{ println("Hello"); println("World"); 21 } }.show)
      println(reify { plus(!'{ println("Hello"); println("World"); 21 }) }.show)
    }

    withQuotes {
      println(reify { test2 }.show)
    }

    withQuotes {
      println(reify { triple(!'{ 21 + 21 }) }.show)
    }

    withQuotes {
      println(reify { !'{ ((x: Int) => ${plus('x)})(100) } }.show)
    }

    withQuotes {
      println(reify { !'{ (y: Int) => {
        println("Hey")
        ${plus(!'{ println("This should not be duplicated"); y })} + ${printPlus('y)}
      } } }.show)
    }
  }
