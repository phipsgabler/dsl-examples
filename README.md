[![No Maintenance Intended](http://unmaintained.tech/badge.svg)](http://unmaintained.tech/)

# DSL-Examples #

Here I implement various showcases of Scala features, mostly related to DSLs, to
illustrate my bachelor's thesis.

Every example consists of a module object, containing all necessary definitions,
and a "test object" (inheriting from `App`), in which calls or constructions are
shown to typecheck or to work.

The "tests" should not be taken too seriously; they are only there to show how
the defined features can be used, not to ensure any invariants. Sometimes there aren't
even any.

## Usage ##

This is an SBT project; simply type `sbt console` on the top level. The package
`dsl_examples` is automatically imported. To play around with individual features,
you can import their module objects, like `import Logic._`.


# Description of Individual Examples #

## [ChurchList](src/main/scala/dsl_examples/ChurchList.scala) ##

An implementation of [church encoded](https://en.wikipedia.org/wiki/Church_encoding) lists, illustrating the hiding
of an internal implementation through an interface by `apply`/`unapply` in the companion object.

The interface is intended to look like the one of `List`: there are members `Empty` and `Cons`, as well as the
universal constructor `ChurchList`, which all can be pattern matched on.

## [Imperative](src/main/scala/dsl_examples/Imperative.scala) ##

Methods `repeat` and `_while`, to illustrate how blocks and by-name arguments can be used to construct functions
which look and behave like imperative language statements.

## [Logic](src/main/scala/dsl_examples/Logic.scala) ##

A small AST for predicate logic expressions, including an implementation function. Illustrates the use of operators and
extractors.

## [MutableDict](src/main/scala/dsl_examples/MutableDict.scala) ##

A mutable dictionary implementation based on a purely functional internal implementation, illustrating the getter/setter
syntax of Scala.

## [Range](src/main/scala/dsl_examples/Range.scala) ##

A wrapper for `Int`, reimplementing the syntax from
[`scala.collection.immutable.Range`](http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Range).

## [Read](src/main/scala/dsl_examples/Read.scala) ##

A simple example of a type class (inspired by Haskell's
[`Read`](http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#t:Read)), providing the functionality of
reading a value of a type from a string.

## [Recover](src/main/scala/dsl_examples/Recover.scala) ##

A reimplementation of [`scala.util.Try`](http://www.scala-lang.org/api/current/index.html#scala.util.Try), illustrating
a usage of by-name parameters.

## [SExpParser](src/main/scala/dsl_examples/SExpParser.scala) ##

A small parser for [S-expressions](https://en.wikipedia.org/wiki/S-expression) (LISP syntax), using Scala's
parser combinators.

## [Delay](src/main/scala/dsl_examples/delay) ##

Multiple versions of delay objects (similar to [futures](https://en.wikipedia.org/wiki/Futures_and_promises), but not
actually using multi-threading).

- `Delay1` uses a mutable variable and hides its internal state.
- `Delay2` does in principle the same, but uses the Scala constructs for by-name arguments and lazy values.
- `Delay3` factors out the interface into a trait, and provides a monadic interface.
- `Delay4` is very similar to `Delay3`, but implements the monad type class from
  [`Scalaz`](https://github.com/scalaz/scalaz).

# License #

This stuff is available under the MIT license. See LICENSE.txt for more info.
