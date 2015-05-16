package dsl_examples

object MutableDict {

  /** Purely functional implementation of a dictionary. */
  private case class DictImpl[T](f: String => Option[T]) {
    def updated(newKey: String, newValue: T) = DictImpl {
      case `newKey`  => Option(newValue)
      case key => get(key)
    }

    def get(key: String): Option[T] = f(key)
  }

  private object DictImpl {
    def empty[T] = DictImpl[T](_ => None)
  }

  class MutableDict[T](private[this] var c : Int) {
    private[this] var dict: DictImpl[T] = DictImpl.empty
    private[this] var size: Int = 0

    /** Updates the internal pure dictionary, if allowed and necessary */
    def update(key: String, value: T): Unit = {
      if (dict.get(key).isEmpty) {
        require(size < capacity, "Capacity exceeded!")
        size += 1
      }

      dict = dict.updated(key, value)
    }

    def apply(key: String): Option[T] = dict.get(key)

    def capacity: Int = c
    def capacity_=(newCapacity: Int): Unit = {
      require(newCapacity >= size, "Capacity too small!")
      c = newCapacity
    }
  }
}

object MutableDictTest extends App {
  import MutableDict._

  val d = new MutableDict[Int](2)
  d("test") = 1
  d("bar") = 42
  println(d("foo"))

  println(d("bar"))
  println(d("foo"))
}
