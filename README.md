# DSL-Examples #

Here I implement various showcases of Scala features, mostly related to DSLs, to
illustrate my bachelor's thesis.

Every example consists of a module object, containing all necessary definitions,
and a "test object" (inheriting from `App`), in which calls or constructions are
shown to typecheck or to work.

The "tests" should not be taken too seriously; they are only there to show how
the defined features can be used, not to ensure any invariants.

## Usage ##

This is an SBT project; simply type `sbt console` on the top level. The package
`dsl_examples` is automatically imported. To play around with individual features,
you can import their module objects, like `import Logic._`.


# Description of Individual Examples #

## Imperative ##

## Logic ##

## MutableDict ##

## Range ##

## Read ##

## Recover ##

## Delay ##
