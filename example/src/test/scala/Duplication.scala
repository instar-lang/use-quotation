import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import useQuotation.Runtime.{reify, run}
import useQuotation.Syntax.*

import munit.FunSuite

class DuplicationTests extends FunSuite with Base:
  given Compiler = Compiler.make(getClass.getClassLoader)

  test("duplication - mention-quotation") {
    def plus(x: Expr[Int])(using Quotes): Expr[Int] =
      '{ $x + $x }
    val (_, out) = captureOut {
      // Here we need to use ordinary mention-quotation.
      staging.run(plus('{ println("dup this"); 42 }))
    }
    withQuotes { print(plus('{ println("dup this"); 42 }).show) }

    assertEquals(out(0), "dup this")
    assertEquals(out(1), "dup this")
  }

  test("duplication - mix use/mention-quotation") {
    def plus(x: Expr[Int])(using Quotes): Expr[Int] =
      !'{ $x + $x }
    val (_, out) = captureOut {
      // As long as we are using mention-quotation, duplication will occur despite the splice-site.
      run(plus('{ println("dup this"); 42 }))
    }
    withQuotes { print(reify { plus('{ println("dup this"); 42 }) }.show) }

    assertEquals(out(0), "dup this")
    assertEquals(out(1), "dup this")
  }

  test("no duplication 1") {
    def plus(x: Expr[Int])(using Quotes): Expr[Int] =
      '{ $x + $x }
    val (_, out) = captureOut {
      //               ↓ use !'{ ... }
      run(plus(!'{ println("no dup"); 42 }))
    }
    assertEquals(out.size, 1)
    assertEquals(out(0), "no dup")
  }

  test("no duplication 2") {
    def plus(x: Expr[Int])(using Quotes): Expr[Int] =
      !'{ $x + $x }
    val (_, out) = captureOut {
      //               ↓ use !'{ ... }
      run(plus(!'{ println("no dup"); 42 }))
    }
    assertEquals(out.size, 1)
    assertEquals(out(0), "no dup")
  }
