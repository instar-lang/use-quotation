# Using Quotations

Experiment with an alternative quotation semantics.

Use ordinary `'{ ... }` for Scala quotation, and `!'{ ... }` for automatic let insertion:

```scala
import scala.quoted.*
import useQuotation.Syntax.*
import useQuotation.Runtime.run

def plus(x: Expr[Int])(using Quotes): Expr[Int] = !'{ $x + $x }

// With a given scala.quoted.staging.Compiler:
val answer = run { plus(!'{ println("Hello"); 21 }) }
// answer == 42; Hello is printed once.
```

Both forms share `Expr[T]` and Scala's splice syntax. Mark each alternative
quotation explicitly; ordinary quotes keep native semantics.

Try examples:

```sh
sbt "example/run"
sbt "example/test"
```
