package dsl_examples

/** A remade implementation of scala.util.Try, using the same means as the "delay" pattern. */
object Recover {
  sealed trait Recover[+A] { self =>
    def map[B](f: A => B): Recover[B] = self match {
      case Success(v) => Success(f(v))
      case Failure(e) => Failure(e)
    }

    def flatMap[B](f: A => Recover[B]): Recover[B] = self match {
      case Success(v) => f(v)
      case Failure(e) => Failure(e)
    }

    def foreach(action: A => Unit): Unit = self match {
      case Success(v) => action(v)
      case Failure(e) =>
    }
  }

  case class Success[A](value: A) extends Recover[A]

  case class Failure[A](error: Throwable) extends Recover[A]

  object Recover {
    def apply[A](tried: => A): Recover[A] = {
      try {
        Success(tried)
      }
      catch {
        case scala.util.control.NonFatal(e) => Failure(e)
      }
    }
  }
}

object RecoverTest extends App {
  import Recover.{Recover => Rec}

  Rec("hello!") foreach println
  Rec(throw new Exception) foreach (_ => println("you should not see this!"))


  // A somehow useful applicative syntax, but quite abusing reflective types. The type of
  // `join _` is:
  // dsl_examples.delay.try.Try[Nothing] => (dsl_examples.delay.try.Try[Nothing] =>
  // AnyRef{def by[C](f: (Nothing, Nothing) => C): dsl_examples.delay.try.Try[C];
  // def apply[C](c: dsl_examples.delay.try.Try[C]): AnyRef{def by[D](f: (Nothing, Nothing, C) => D):
  // dsl_examples.delay.try.Try[D]; def apply[D](d: dsl_examples.delay.try.Try[D]):
  // AnyRef{def by[E](f: (Nothing, Nothing, C, D) => E): dsl_examples.delay.try.Try[E]}}})
  //
  // yay! ;)

  def join[A, B](a: Rec[A])(b: Rec[B]) = new {
    def by[C](f: (A, B) => C): Rec[C] = for {
      x <- a
      y <- b
    } yield f(x, y)

    def apply[C](c: Rec[C]) = new {
      def by[D](f: (A, B, C) => D): Rec[D] = for {
        x <- a
        y <- b
        z <- c
      } yield f(x, y, z)

      def apply[D](d: Rec[D]) = new {
        def by[E](f: (A, B, C, D) => E): Rec[E] = for {
          x <- a
          y <- b
          z <- c
          w <- d
        } yield f(x, y, z, w)

        // probably enough for practical purposes...
      }
    }
  }
}