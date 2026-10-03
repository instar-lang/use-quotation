import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import useQuotation.Runtime.{reify, run}

object Example:
  given Compiler = Compiler.make(getClass.getClassLoader)

  // def bad(using Quotes): Expr[Quotes ?=> Expr[Int]] = '{ '{ 1 } }

  def id(x: Expr[Int])(using Quotes): Expr[Int] = x

  def test1 =
    val answer = run { '{ ((x: Int) => x)(42) } }
    assert(answer == 42)
    println(s"result: $answer")

  def plus(x: Expr[Int])(using Quotes): Expr[Int] =
    '{ $x + $x }

  def triple(x: Expr[Int])(using Quotes): Expr[Int] =
    '{ $x + $x + $x }

  def printPlus(x: Expr[Int])(using Quotes): Expr[Int] =
    val code = '{ println("This should not be discarded") }
    '{ $x + $x }

  def test2(using Quotes) =
    val value = '{ 7 }
    val spliced = '{ ((x: Int) => x)($value) }
    spliced

  def main(args: Array[String]): Unit =
    assert(run { plus('{ println("Hello"); 21 }) } == 42)
    withQuotes {
      println(reify { '{ println("Hello"); println("World"); 21 } }.show)
      println(reify { plus('{ println("Hello"); println("World"); 21 }) }.show)
    }

    withQuotes {
      println(reify { test2 }.show)
    }

    withQuotes {
      println(reify { triple('{ 21 + 21 }) }.show)
    }

    withQuotes {
      println(reify { '{ ((x: Int) => ${plus('x)})(100) } }.show)
    }

    withQuotes {
      println(reify { '{ (y: Int) => {
        println("Hey")
        ${plus('{ println("This should not be duplicated"); y })} + ${printPlus('y)}
      } } }.show)
    }

    /*
    test1
    assert(run { plus('{ println("Hello"); 21 }) } == 42)
    */
