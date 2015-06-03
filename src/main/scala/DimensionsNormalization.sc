import scala.annotation.unchecked

sealed trait Dimension {
  def *(other: Dimension) = new *(this, other)
  def /(other: Dimension) = new /(this, other)
}
case object One extends Dimension
case object Meter extends Dimension
case object Second extends Dimension
case object Kilogram extends Dimension
case object Mol extends Dimension
case object Candela extends Dimension
case object Kelvin extends Dimension
case object Ampere extends Dimension
case class *(d1: Dimension, d2: Dimension) extends Dimension
case class /(d1: Dimension, d2: Dimension) extends Dimension

def rank(d: Dimension): Int = d match {
  case One => 1
  case Meter => 2
  case Second => 3
  case Kilogram => 4
  case Mol => 5
  case Candela => 6
  case Kelvin => 7
  case Ampere => 8
  case (One / (One / _)) => 10
  case (One / x) if rank(x) < 9 => rank(x)
  case _ => 10
}
def prim(d: Dimension) = rank(d) < 10
def inv(d: Dimension) = rank(d) == 9
def normalize(d: Dimension): Dimension = reorder(simplify(d))
def simplify(d: Dimension): Dimension = d match {
  case e if prim(e) => e

  case One * b => simplify(b)
  case e@(a * b) if prim(a) && prim(b) => e
  case a * b if prim(a) => a * simplify(b)

  case a / One => simplify(a)
  case One / (One / b) => simplify(b)
  case e@(a / b) if prim(a) && prim(b) => a * (One / b)
  case a / b if prim(a) => a * simplify(invert(b))

  case (a * b) * c => simplify(a * (b * c))
  case (a / b) * c => simplify(a * (invert(b) * c))
  case (a / b) / c => simplify(a * (invert(b) * invert(c)))
  case (a * b) / c => simplify(a * (b * invert(c)))
}
// expects simplified order
def reorder(d: Dimension): Dimension = (d: @unchecked) match {
  case e if prim(e) => e
  case e@(a * b) if prim(a) && prim(b) => if (rank(a) <= rank(b)) e else b * a
  case a * b if prim(a) => (reorder(b): @unchecked) match {
    case c * d => if (rank(a) <= rank(c) && rank(a) <= rank(d)) a * reorder(c * d) else
                  if (rank(c) <= rank(a)) c * reorder(a * d) else d * reorder(a * c)
  }
}
def invert(d: Dimension): Dimension = d match {
  case One => One
  case e if prim(e) => One / e
  case a * b => (One / a) * (One / b)
  case a / b => b / a
}
simplify(Ampere / (Kilogram * Second))
simplify((Kilogram * Meter) / (Second * Second))
simplify((Ampere * Meter) / (Kelvin * Second))
simplify(One / (Ampere * Mol))
reorder(One / Second)
reorder(Second * Kilogram)
reorder(Ampere * (Kilogram * Meter))
reorder(Ampere * ((One / Kilogram) * (One / Second)))
normalize(Ampere / (Kilogram * Second) * Mol)