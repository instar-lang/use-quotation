package useQuotation

import scala.quoted.Expr

/** Marks a direct quotation for the use-quotation compiler plugin. */
object Syntax:
  extension [T](code: Expr[T])
    def unary_! : Expr[T] =
      throw new IllegalStateException("!'{ ... } requires the use-quotation compiler plugin")
