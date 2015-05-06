package dsl_examples.patterns

//import java.math.BigDecimal
import scala.math._

object complex {
  import Complex._
  type F = Double
  type C = (F, F)



  object Cartesian {
    def apply(a: F, b: F): C = (a, b)
    def unapply(z: C): Option[(F, F)] = Some(z)
  }

  object Polar {
    def apply(r: Double, phi: Double): C = {
      require(!doubleEq(phi, 0.0))
      (r * cos(phi), r * sin(phi))
    }
    def unapply(z: C): Option[(Double, Double)] = z match {
      case (a, b) => Some(sqrt(pow(a, 2) + pow(b, 2)), atan2(b, a))
    }
  }

  object Real {
    def apply(a: Double): C = (a, 0)
    def unapply(z: C): Option[Double] = z match {
      case (a, z) if doubleEq(z, 0) => Some(a)
      case _ => None
    }
  }
//
//  object Imaginary {
//    def apply(b: Double): C = (0, b)
//    def unapply(z: C): Option[Double] = z match {
//      case (0, b) => Some(b)
//      case _ => None
//    }
//  }

//  object I {
//    def unapply
//  }
}

object Complex extends App {
  import complex._

  def doubleEq(a: Double, b: Double): Boolean = {
    val error_factor = 2.0
    (a == b) || abs(a-b) < abs(min(a,b)) * ulp(min(a, b)) * error_factor
  }

  implicit class RichC(val z: C) {


    def =~=(w: C): Boolean = doubleEq(z._1, w._1) && doubleEq(z._2, w._2)
  }
}
