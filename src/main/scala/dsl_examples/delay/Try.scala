package dsl_examples.delay

trait TryImplementation {

  sealed trait Try[+A] { self =>
    def map[B](f: A => B): Try[B] = self match {
      case Success(v) => Success(f(v))
      case Failure(e) => Failure(e)
    }

    def flatMap[B](f: A => Try[B]): Try[B] = self match {
      case Success(v) => f(v)
      case Failure(e) => Failure(e)
    }

    def foreach(action: A => Unit): Unit = self match {
      case Success(v) => action(v)
      case Failure(e) =>
    }
  }

  case class Success[A](value: A) extends Try[A]

  case class Failure[A](error: Throwable) extends Try[A]

  object Try {
    def apply[A](tried: => A): Try[A] = {
      try {
        Success(tried)
      }
      catch {
        case scala.util.control.NonFatal(e) => Failure(e)
      }
    }
  }
}

object Try extends TryImplementation {
  Try("hello!") foreach println
  Try(throw new Exception) foreach (_ => println("you should not see this!"))
}