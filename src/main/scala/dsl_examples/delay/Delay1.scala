package dsl_examples.delay

/** Using explicit mutability, like in scheme. */
object delay1 {
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

object Delay1 extends App {
  import delay1._

  val d1 = new Delayed((_unit) => {
    println("heavy computation")
    2
  })
}