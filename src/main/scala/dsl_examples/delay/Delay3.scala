package dsl_examples.delay

/** Refactor construction, and introduce monadic interface. */
trait Delay3Implementation {
  trait Delayed[+T] {
    import Delayed._

    def force: T

    def map[U](f: T => U): Delayed[U] = delay {
      f(this.force)
    }

    def flatMap[U](f: T => Delayed[U]): Delayed[U] = delay {
      f(this.force).force
    }

    def foreach(f: T => Unit): Unit = f(this.force)
  }

  object Delayed {
    def delay[T](delayed: => T) = new Delayed[T] {
      private[this] lazy val cached: T = delayed
      def force: T = cached
    }
  }
}

object Delay3 extends App with Delay3Implementation {

}
