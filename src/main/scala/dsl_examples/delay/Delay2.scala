package dsl_examples.delay

/** Using scala features to replace explicit mutability. */
object delay2 {
  class Delayed[+T] private (thunk: () => T) {
    private[this] lazy val cached: T = thunk()

    def force: T = cached
  }

  object Delayed {
    def delay[T](delayed: => T) = new Delayed(() => delayed)
  }
}

object Delay2Test extends {
  import delay2._
  import Delayed._

  val d2 = delay {
    println("heavy computation")
    2
  }
}
