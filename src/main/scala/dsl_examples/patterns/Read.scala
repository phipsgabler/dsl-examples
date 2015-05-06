package dsl_examples.patterns

import scala.util.Try

object read {
  trait Read[T] {
    def read(s: String): T
  }

  object Read {

    implicit object stringIsRead extends Read[String] {
      def read(s: String) = s
    }

    implicit object boolIsRead extends Read[Boolean] {
      def read(s: String) = s match {
        case "true" => true
        case "false" => false
      }
    }

    implicit def numericIsRead[N](implicit numN: Numeric[N])
    : Read[N] = new Read[N] {
      def read(s: String) = numN.fromInt(s.toInt)
    }

    implicit def listIsRead[A](implicit readA: Read[A])
    : Read[List[A]] = new Read[List[A]] {
      // this is not a parsing exercise...
      def read(s: String) = s.filter((c: Char) => c != ' ')
        .split(",").toList map readA.read
    }
  }

  implicit class StringReadOps(val self: String) {
    def readOption[T](implicit readT: Read[T]): Option[T] = Try(readT.read(self)).toOption

    def readTry[T](implicit readT: Read[T]): Try[T] = Try(readT.read(self))

    def readAs[T](implicit readT: Read[T]): T = readT.read(self)
  }
}

object ReadTest extends App {
  import read._

  def readAll[T: Read](l: List[String]): List[T] = l map (_.readAs[T])
}