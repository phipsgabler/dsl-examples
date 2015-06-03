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

  class Value[D <: Dimension] private (val value: Double) {
    // multiplication and division produce normalized results, so we
    // don't need to check for addition/subtraction

    def +[E](other: Value[D]) = new Value(value + other.value)

    def -(other: Value[D]) = new Value(value - other.value)

    def *[E](other: Value[E])(implicit normalized: Normalized[D * E]): Value[normalized.result] =
      new Value(value * other.value)

    def /[E](other: Value[E])(implicit normalized: Normalized[D / E]): Value[normalized.result] =
      new Value(value / other.value)
  }

  implicit class dimensionSymbols(d: Double) {
    def meters = new Value[Meter](d)
    def seconds = new Value[Second](d)
    def kilograms = new Value[Second](d)
    def mols = new Value[Second](d)
    def candelas = new Value[Second](d)
    def kelvins = new Value[Second](d)
    def amperes = new Value[Second](d)

    def meter = new Value[Meter](d)
    def second = new Value[Second](d)
    def kilogram = new Value[Second](d)
    def mol = new Value[Second](d)
    def candela = new Value[Second](d)
    def kelvin = new Value[Second](d)
    def ampere = new Value[Second](d)
  }

  implicit def dimensionless(d: Double): Value[One] = new Value[One](d)
}

object UnitsTest extends App {
  import Units._

  println((1.0 meter) * (2.0 meters))
}



private[dsl_examples] trait Normalizations {
  import Units._
  // always convert to form m^a * s^b * kg^c * mol^d * Cd^e * K^f * A^f, or One

  implicit def meterProduct[D](implicit dNorm: Normalized[D]): Normalized[Meter * D] = new Normalized[Meter * D] {
    type result = dNorm.result
  }

  implicit def multiplyByOne[D](implicit dNorm: Normalized[D]): Normalized[One * D] = new Normalized[One * D] {
    type result = dNorm.result
  }

  implicit def divideByOne[D](implicit dNorm: Normalized[D]): Normalized[D / One] = new Normalized[D / One] {
    type result = dNorm.result
  }

  implicit def divideBySelf[D](implicit dNorm: Normalized[D]): Normalized[D / D] = new Normalized[D / D] {
    type result = One
  }
}

























