package Delay

trait Delay2Implementation {
  class Delayed[+T](thunk: () => T) {
    private[this] lazy val cached: T = thunk()

    def force: T = cached
  }

  object Delayed {
    def delay[T](delayed: => T) = new Delayed(() => delayed)
  }
}

object Delay2 extends App with Delay2Implementation {

}
