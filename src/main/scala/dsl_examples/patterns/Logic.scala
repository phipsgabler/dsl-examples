package dsl_examples.patterns

object logic {
  sealed trait Expr {
    def |(other: Expr) = Or(this, other)
    def &(other: Expr) = And(this, other)
    def unary_~ = Not(this)

    def eval(env: Map[String, Boolean]): Option[Boolean] = this match {
      case Var(s) => env.get(s)
      case Or(a, b) => for (x <- a.eval(env); y <- b.eval(env)) yield x || y
      case And(a, b) => for (x <- a.eval(env); y <- b.eval(env)) yield x && y
      case Not(a) => a.eval(env).map(!_)
    }
  }

  case class Var(s: String) extends Expr
  case class Or(a: Expr, b: Expr) extends Expr
  case class And(a: Expr, b: Expr) extends Expr
  case class Not(a: Expr) extends Expr

  implicit def stringToExpr(s: String): Var = Var(s)
}

object LogicTest {
  import logic._

  val e1: Expr = "a" | "b" & ~"c"
  assert(e1.eval(Map("a" -> true, "b" -> false, "c" -> true)) == Some(true))
}
