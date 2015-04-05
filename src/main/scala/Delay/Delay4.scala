package Delay

import scalaz._

trait Delay4Implementation {
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

      def bind[U, V](du: Delayed[U])(f: U => Delayed[V]): Delayed[V] = delay {
        f(du.force).force
      }
    }

    implicit class toDelayOps[T](value: => T) {
      def delayed: Delayed[T] = delay(value)
    }
  }
}

object Delay4 extends App with Delay4Implementation {
  import Delayed._

  import Scalaz._

  val first: Delayed[Int] = 1.delayed
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
