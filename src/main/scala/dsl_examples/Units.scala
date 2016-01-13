package dsl_examples

import scala.language.postfixOps

object Units extends Normalizations {
  sealed trait Dimension

  final class One extends Dimension
  final class Meter extends Dimension
  final class Second extends Dimension
  final class Kilogram extends Dimension
  final class Mol extends Dimension
  final class Candela extends Dimension
  final class Kelvin extends Dimension
  final class Ampere extends Dimension


  final class *[A <: Dimension, B <: Dimension] extends Dimension
  final class /[A <: Dimension, B <: Dimension] extends Dimension

  private[dsl_examples] abstract class Normalized[D <: Dimension] {
    type result <: Dimension
  }

  class Value[D <: Dimension] private[Units] (val value: Double) {
    // multiplication and division produce normalized results, so we
    // don't need to check for addition/subtraction

    def +(other: Value[D]) = new Value(value + other.value)

    def -(other: Value[D]) = new Value(value - other.value)

    def *[E <: Dimension](other: Value[E])(implicit normalized: Normalized[D * E]): Value[normalized.result] =
      new Value(value * other.value)

    def /[E <: Dimension](other: Value[E])(implicit normalized: Normalized[D / E]): Value[normalized.result] =
      new Value(value / other.value)


  }

  implicit class dimensionSymbols(d: Double) {
    def meters = new Value[Meter](d)
    def seconds = new Value[Second](d)
    def kilograms = new Value[Kilogram](d)
    def mols = new Value[Mol](d)
    def candelas = new Value[Candela](d)
    def kelvins = new Value[Kelvin](d)
    def amperes = new Value[Ampere](d)

    def meter = new Value[Meter](d)
    def second = new Value[Second](d)
    def kilogram = new Value[Kilogram](d)
    def mol = new Value[Mol](d)
    def candela = new Value[Candela](d)
    def kelvin = new Value[Kelvin](d)
    def ampere = new Value[Ampere](d)

    def of[D <: Dimension] = new Value[D](d)
  }

  implicit def dimensionless(d: Double): Value[One] = new Value[One](d)
}

object UnitsTest extends App {
  import Units._

  //println((1.0 meter) * (2.0 meters))
}



trait Normalizations {
  import Units._
  // always convert to form m^a * s^b * kg^c * mol^d * Cd^e * K^f * A^f, or One

  // type-level ordering on dimensions
  abstract class LEQ[D <: Dimension, E <: Dimension]
  implicit object oneLeqMeter extends LEQ[One, Meter]
  implicit object meterLeqSecond extends LEQ[Meter, Second]
  implicit object secondLeqKilogram extends LEQ[Second, Kilogram]
  implicit object kilogramLeqMol extends LEQ[Kilogram, Mol]
  implicit object molLeqCandela extends LEQ[Mol, Candela]
  implicit object candelaLeqKelvin extends LEQ[Candela, Kelvin]
  implicit object kelvinLeqAmpere extends LEQ[Kelvin, Ampere]
  implicit def primLeqInverseSimple[D <: Dimension](implicit dIsPrim: Prim[D]): LEQ[D, One / D] = new LEQ[D, One / D] {}
  implicit def rest[D <: Dimension, E <: Dimension](implicit dIsSimple: Simple[D]): LEQ[D, E] = new LEQ[D, E] {}
  implicit def leqTransitive[A <: Dimension, B <: Dimension, C <: Dimension]
    (implicit aLeqB: LEQ[A, B], bLeqC: LEQ[B, C]): LEQ[A, C] = new LEQ[A, C] {}

  abstract class Prim[D <: Dimension]
  implicit object meterIsPrimitive extends Prim[Meter]
  implicit object secondIsPrimitive extends Prim[Second]
  implicit object kilogramIsPrimitive extends Prim[Kilogram]
  implicit object molIsPrimitive extends Prim[Mol]
  implicit object candelaIsPrimitive extends Prim[Candela]
  implicit object kelvinIsPrimitive extends Prim[Kelvin]
  implicit object ampereIsPrimitive extends Prim[Ampere]

  abstract class Simple[D <: Dimension]
  implicit def primIsSimple[D <: Dimension](implicit dIsPrim: Prim[D]): Simple[D] = new Simple[D] {}
  implicit def inversePrimIsSimple[D <: Dimension](implicit dIsPrim: Prim[D]): Simple[One / D] = new Simple[One / D] {}


  // type-level inversion of dimensions
  abstract class Inverted[D <: Dimension] {
    type result <: Dimension
  }

  implicit object oneInverted extends Inverted[One] { type result = One }
  implicit def primInverted[D <: Dimension](implicit dIsPrim: Prim[D]): Inverted[D] =
    new Inverted[D] { type result = One / D }
  implicit def multInverted[D <: Dimension, E <: Dimension]: Inverted[D * E] =
    new Inverted[D * E] { type result = (One / D) * (One / E) }
  implicit def divInverted[D <: Dimension, E <: Dimension]: Inverted[D / E] =
    new Inverted[D / E] { type result = E / D }
//
//
  // type-level simplification of dimensions
  abstract class Simplified[D <: Dimension] {
    type result <: Dimension
  }

  implicit def simpleSimplified[D <: Dimension](implicit dIsSimple: Simple[D]): Simplified[D] =
    new Simplified[D] { type result = D }
  implicit def oneMultDSimplified[D <: Dimension]: Simplified[One * D] =
    new Simplified[One * D] { type result = D }
  implicit def simpleMultSimpleSimplified[D <: Dimension, E <: Dimension]
  (implicit dIsSimple: Simple[D], eIsSimple: Simple[E]): Simplified[D * E] =
    new Simplified[D * E] { type result = D * E }
  implicit def simpleMultComplexSimplified[D <: Dimension, E <: Dimension]
  (implicit dIsSimple: Simple[D], eSimplified: Simplified[E]): Simplified[D * E] =
    new Simplified[D * E] { type result = D * eSimplified.result }

  implicit def aDivOneSimplified[D <: Dimension](implicit dSimplified: Simplified[D]): Simplified[D / One] =
    new Simplified[D / One] { type result = dSimplified.result }
  implicit def oneDivOneDivDSimplified[D <: Dimension](implicit dSimplified: Simplified[D]): Simplified[One / (One / D)] =
    new Simplified[One / (One / D)] { type result = dSimplified.result }
  implicit def simpleDivSimpleSimplified[D <: Dimension, E <: Dimension]
  (implicit dIsSimple: Simple[D], eIsSimple: Simple[E]): Simplified[D / E] =
    new Simplified[D / E] { type result = D / E }
//  implicit def simpleDivComplexSimplified[D <: Dimension, E <: Dimension](implicit dIsSimple: Simple[D], eInverted: Inverted[E], invertedSimplified: Simplified[eInverted.result]): Simplified[D * E] =
//    new Simplified[D * E] { type result = D * invertedSimplified.result }
  implicit def simpleDivComplexSimplified[D <: Dimension, E <: Dimension, InvertedE <: Dimension](implicit dIsSimple: Simple[D], eInverted: Inverted[E] { type result = InvertedE }, eSimplifiedInverted: Simplified[InvertedE]): Simplified[D * E] =
    new Simplified[D * E] { type result = D * eSimplifiedInverted.result }

  implicit def aMultBMultC[A <: Dimension, B <: Dimension, C <: Dimension](implicit simplified: Simplified[*[A, *[B, C]]]): Simplified[*[*[A, B], C]] =
    new Simplified[*[*[A, B], C]] { type result = simplified.result }
  implicit def aMultBMultC[A <: Dimension, B <: Dimension, C <: Dimension, InvertedB <: Dimension](implicit invertedB: Inverted[B] { type result = InvertedB }, simplified: Simplified[*[A, *[InvertedB, C]]]): Simplified[*[/[A, B], C]] =
    new Simplified[*[/[A, B], C]] { type result = simplified.result }
  implicit def aMultBMultC[A <: Dimension, B <: Dimension, C <: Dimension, InvertedB <: Dimension, InvertedC <: Dimension](implicit invertedB: Inverted[B] { type result = InvertedB }, invertedC: Inverted[C] { type result = InvertedC }, simplified: Simplified[*[A, *[InvertedB, InvertedC]]]): Simplified[/[/[A, B], C]] =
    new Simplified[/[/[A, B], C]] { type result = simplified.result }
  implicit def aMultBMultC[A <: Dimension, B <: Dimension, C <: Dimension, InvertedC <: Dimension](implicit invertedC: Inverted[C] { type result = InvertedC }, simplified: Simplified[*[A, *[B, InvertedC]]]): Simplified[/[*[A, B], C]] =
    new Simplified[/[*[A, B], C]] { type result = simplified.result }
//
//
//  // type-level sorting of already simplified dimensions
//  abstract class Reordered[D <: Dimension] {
//    type result <: Dimension
//  }
//
//  implicit def simpleReordered[D <: Dimension](implicit dIsSimple: Simple[D]): Reordered[D] =
//    new Reordered[D] { type result = D }
//  implicit def simpleMultSimpleReordered[D <: Dimension, E <: Dimension](implicit dIsSimple: Simple[D], eIsSimple: Simple[E], dLeqE: LEQ[D, E]): Reordered[D * E] =
//    new Reordered[D * E] { type result = D * E }
//  implicit def simpleMultSimpleReordered[D <: Dimension, E <: Dimension](implicit dIsSimple: Simple[D], eIsSimple: Simple[E], eLeqD: LEQ[E, D]): Reordered[D * E] =
//    new Reordered[D * E] { type result = E * D }
//
//  implicit def simpleMultComplexReordered[D <: Dimension, E <: Dimension, F <: Dimension](implicit dIsSimple: Simple[D], dLeqE: LEQ[D, E], dLeqF: LEQ[D, F], efReorderded: Reordered[E * F]): Reordered[*[D, *[E, F]]] =
//    new Reordered[*[D, *[E, F]]] { type result = *[D, efReorderded.result] }
//  implicit def simpleMultComplexReordered[D <: Dimension, E <: Dimension, F <: Dimension](implicit dIsSimple: Simple[D], eLeqD: LEQ[E, D], dLeqF: LEQ[D, E], dfReorderded: Reordered[D * F]): Reordered[*[D, *[E, F]]] =
//    new Reordered[*[D, *[E, F]]] { type result = *[E, dfReorderded.result] }
//  implicit def simpleMultComplexReordered[D <: Dimension, E <: Dimension, F <: Dimension](implicit dIsSimple: Simple[D], dLeqE: LEQ[D, E], fLeqD: LEQ[F, D], deReorderded: Reordered[D * E]): Reordered[*[D, *[E, F]]] =
//    new Reordered[*[D, *[E, F]]] { type result = *[F, deReorderded.result] }
//
//  implicit def simpleMultComplexReordered[D <: Dimension, E <: Dimension, F <: Dimension](implicit dIsSimple: Simple[D], eLeqD: LEQ[E, D], fLeqD: LEQ[F, D], eLeqF: LEQ[E, F], dfReorderded: Reordered[D * F]): Reordered[*[D, *[E, F]]] =
//    new Reordered[*[D, *[E, F]]] { type result = *[E, dfReorderded.result] }
//  implicit def simpleMultComplexReordered[D <: Dimension, E <: Dimension, F <: Dimension](implicit dIsSimple: Simple[D], eLeqD: LEQ[E, D], fLeqD: LEQ[F, D], fLeqE: LEQ[F, E], deReorderded: Reordered[D * E]): Reordered[*[D, *[E, F]]] =
//    new Reordered[*[D, *[E, F]]] { type result = *[F, deReorderded.result] }
//
//
//  implicit def normalized[D <: Dimension, SimplifiedD <: Dimension](implicit simplified: Simplified[D] { type result = SimplifiedD }, reorderedSimplified: Reordered[SimplifiedD]): Normalized[D] =
//    new Normalized[D] { type result = reorderedSimplified.result }
}

object Normalizations extends Normalizations

























