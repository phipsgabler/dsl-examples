package dsl_examples.delay

import scalaz._

/** Implement the scalaz monad type class. */
object Delay4 {
  trait Delayed[+T] {
    def force: T
  }

  object Delayed {
    def delay[T](delayed: => T) = new Delayed[T] {
      private[this] lazy val cached: T = delayed
      def force: T = cached
    }

    implicit object DelayedMonad extends Monad[Delayed] {
      def point[T](t: => T): Delayed[T] = delay(t)

      def bind[U, V](du: Delayed[U])(f: U => Delayed[V]) = delay {
        f(du.force).force
      }
    }

    implicit class toDelayOps[T](value: => T) {
      def delayed: Delayed[T] = delay(value)
      def strict: Delayed[T] = new Delayed[T] {
        def force: T = value
      }
    }
  }
}

object Delay4Test extends App {
  import Delay4._
  import Delayed._

  import Scalaz._

  val first = 1.delayed
  val second = delay {
    println("hello")
    2
  }

  val a = (first |@| second)(_ + _)

  // should be equal to:

  val b = for {
    x <- first
    y <- second
  } yield x + y

  assert(a.force === b.force) // only here we should see "hello"
}
