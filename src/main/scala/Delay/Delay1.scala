package Delay

trait Delay1Implementation {
  class Delayed[+T](thunk: Unit => T) {
    private[this] var cached: Option[T] = None

    def force: T = cached match {
      case Some(value) => value
      case None => {
        val value = thunk(())
        cached = Some(value)
        value
      }
    }
  }
}

object Delay1 extends App with Delay1Implementation {

}
