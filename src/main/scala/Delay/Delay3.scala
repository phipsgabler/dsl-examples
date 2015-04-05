package Delay

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
  }

  object Delayed {
    def delay[T](delayed: => T) = new Delayed[T] {
      private[this] lazy val cached: T = delayed
      def force: T = cached
    }
    def pure[T](t: T) = new Delayed[T] {
      override def force = t
    }
  }
}

object Delay3 extends App with Delay3Implementation {

}
