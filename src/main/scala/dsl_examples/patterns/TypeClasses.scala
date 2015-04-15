package dsl_examples.patterns

import scala.util.Try

trait Read[T] {
  def read(s: String): T
}

trait ReadSyntaxImpl {
  import ReadSyntax._

  implicit def numericIsRead[N](implicit numN: Numeric[N]): Read[N] = new Read[N] {
    def read(s: String) = numN.fromInt(s.toInt)
  }

  implicit object stringIsRead extends Read[String] {
    def read(s: String) = s
  }

  implicit  def listIsRead[A](implicit readA: Read[A]): Read[List[A]] = new Read[List[A]] {
    // this is not a parsing exercise...
    def read(s: String) = s.filter((c: Char) => c != ' ').split(",").toList map (_.readAs[A])
  }
}

object ReadSyntax extends ReadSyntaxImpl {
  implicit class StringOps(val self: String) extends AnyVal
  {
    def readOption[T](implicit readT: Read[T]): Option[T] = Try(readT.read(self)).toOption
    def readTry[T](implicit readT: Read[T]): Try[T] = Try(readT.read(self))
    def readAs[T](implicit readT: Read[T]): T = readT.read(self)
  }
}