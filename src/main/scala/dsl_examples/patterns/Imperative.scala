package dsl_examples.patterns

object imperative {
  def _while(condition: => Boolean)(body: => Unit): Unit = {
    if (condition) { body; _while(condition)(body) }
  }
}

object ImperativeTest extends App {
  import imperative._

  var x = 10
  _while(x > 0) {
    println(x)
    x -= 1
  }
}
