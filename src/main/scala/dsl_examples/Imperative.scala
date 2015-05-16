package dsl_examples

object Imperative {
  def repeat(n: Int)(block: => Any): Unit =
    if (n > 0) {
      block
      repeat(n - 1)(block)
    }

  def _while(condition: => Boolean)(body: => Unit): Unit = {
    if (condition) { body; _while(condition)(body) }
  }
}

object ImperativeTest extends App {
  import Imperative._

  repeat(5) {
    println("hello!")
  }


  var x = 10
  _while(x > 0) {
    println(x)
    x -= 1
  }
}
