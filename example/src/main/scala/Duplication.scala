import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}
import useQuotation.Runtime.{reify, run}

object Duplication:

  given Compiler = Compiler.make(getClass.getClassLoader)

  def plus(x: => Expr[Int])(using Quotes): Expr[Int] =
    '{ $x + $x }

  def main(args: Array[String]): Unit =
    withQuotes {
      println(reify { plus('{ println("dup this"); 42 }) }.show)
    }