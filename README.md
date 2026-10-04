# Using Quotations

Quotation in multi-stage programming supports two different intentions:
(1) constructing program syntax and (2) deferring an existing computation.
However, conventional Lisp-style quotation found in Scala 3, MetaOCaml, MetaML,
etc. directly supports the former, but does not by itself guarantee the latter.

Such a distinction mirrors the use-mention distinction in logic and philosophy. When we say

> Snow is white

we **use** the word "snow" to talk about snow. But in

> "Snow" has four letters

we **mention** the word to talk about its form.
However, more often [quotation mixes use and mention](https://homes.luddy.indiana.edu/ccshan/quote/characterizing.pdf):

> Quine says quotation "has a certain anomalous feature".

The quoted words are attributed to Quine, but they also function as a verb phrase and are part of the meaning of the sentence, contributing what is said about quotation.

Compare the direct quotation

> Quine says "quotation has a certain anomalous feature".

it presents the entire clause as quoted wording, **without using** it.

**This Scala compiler plugin experiments with an alternative
use-quotation semantics in staged programming where quoted terms are always used.
Quotation is an instruction to residualize a computation, rather than merely constructing syntax.**

### Usage

Use ordinary `'{ ... }` for Scala quotation, and `!'{ ... }` for semantics preservation via automatic let-insertion:

```scala
import scala.quoted.*
import useQuotation.Syntax.*
import useQuotation.Runtime.run

def plus(x: Expr[Int])(using Quotes): Expr[Int] = '{ $x + $x }

// With a given scala.quoted.staging.Compiler:

val answer = run { plus(!'{ println("Hello"); 21 }) }
// answer == 42; Hello is printed once.
```

The above code generates the following residual program:

```scala
val x1 = println("Hello")
val x2 = 21
x2 + x2
```
However, using ordinary mention-quotation `run { plus('{ println("Hello"); 21 }) }` will generate code
```
{ println("Hello"); 21 } + { println("Hello"); 21 }
```

Both forms share `Expr[T]` and Scala's splice syntax. Mark each alternative
quotation explicitly; ordinary quotes keep native semantics.

Try examples:

```sh
sbt "example/run"
sbt "example/test"
```
