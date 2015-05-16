package dsl_examples.delay

/** Using scala features to replace explicit mutability. */
object Delay2 {
  class Delayed[+T] private (thunk: () => T) {
    private[this] lazy val cached: T = thunk()

    def force: T = cached
  }

  object Delayed {
    def delay[T](delayed: => T) = new Delayed(() => delayed)
  }
}

object Delay2Test extends {
  import Delay2._
  import Delayed._

  val d1 = delay {
    println("doing something")
    1
  }

  val d2 = delay {
    println("heavy computation")
    2
  }

  val added = delay {
    d1.force + d2.force
  }
}
