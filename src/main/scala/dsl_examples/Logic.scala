package dsl_examples

import scala.language.reflectiveCalls

object Logic {
  sealed trait Expr {
    def ||(other: Expr) = Or(this, other)
    def &&(other: Expr) = And(this, other)
    def unary_! = Not(this)

    def eval(env: Map[String, Boolean]): Option[Boolean] = this match {
      case Var(s) => env.get(s)
      case a || b => join(a.eval(env), b.eval(env)) by (_ || _) //for (x <- a.eval(env); y <- b.eval(env)) yield x || y
      case a && b => join(a.eval(env), b.eval(env)) by (_ && _) //for (x <- a.eval(env); y <- b.eval(env)) yield x && y
      case !(a) => a.eval(env).map(!_)
    }
  }

  case class Var(s: String) extends Expr
  case class Or(a: Expr, b: Expr) extends Expr
  case class And(a: Expr, b: Expr) extends Expr
  case class Not(a: Expr) extends Expr

  implicit def stringToExpr(s: String): Var = Var(s)

  object || {
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case Or(a, b) => Option(a, b)
      case _ => None
    }
  }

  object && {
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case And(a, b) => Option(a, b)
      case _ => None
    }
  }

  object ! {
    def unapply(e: Expr): Option[Expr] = e match {
      case Not(a) => Option(a)
      case _ => None
    }
  }

  // Quick and dirty; requires `reflectiveCalls`. Alternative: |@| from `scalaz`.
  def join[A, B](a: Option[A], b: Option[B]) = new {
    def by[C](f: (A, B) => C): Option[C] = for {
      x <- a
      y <- b
    } yield f(x, y)
  }
}

object LogicTest extends App {
  import Logic._

  val e1: Expr = "a" || "b" && !"c"
  assert(e1.eval(Map("a" -> true, "b" -> false, "c" -> true)) == Some(true))

  e1 match {
    case e@(Var("a") || Var("b") && !(Var("c"))) => println(s"Success: $e")
    case _ => println("Failure")
  }
}
