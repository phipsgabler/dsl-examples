package dsl_examples

object ChurchList {
  sealed trait ChurchList[+T] {
    def fold[K]: K => (T => K => K) => K
    def toList: List[T] = fold[List[T]](Nil)(x => xs => x::xs)
    def isEmpty: Boolean = this.fold(true)(_ => _ => false)
    override def toString = s"ChurchList(${this.toList.mkString(",")})"
  }

  object ChurchList {
    def apply[T](l: T*): ChurchList[T] = l.toList match {
      case Nil => Empty
      case x::xs => new ChurchList[T] {
        val rest = ChurchList[T](xs: _*)
        def fold[K]: K => (T => K => K) => K =
          nil => plus => plus(x)(rest.fold(nil)(plus))
      }
    }

    def unapplySeq[T](l: ChurchList[T]): Option[Seq[T]] =
      Some(l.fold[List[T]](Nil)(x => xs => x::xs))
  }

  object Empty extends ChurchList[Nothing] {
    def fold[K] = nil => _ => nil
    override def toString = "Empty"
  }

  object Cons {
    def apply[T](x: T, xs: ChurchList[T]) = new ChurchList[T] {
      def fold[K]: K => (T => K => K) => K =
        nil => plus => plus(x)(xs.fold(nil)(plus))
    }

//    def unapply[T](l: ChurchList[T]): Option[(T, ChurchList[T])] = {
//      l.fold[Option[(T, ChurchList[T])]](None)(h => acc => acc match {
//        case Some((x, xs)) => Some((x, Cons(h, xs)))
//        case None => Some((h, Empty))
//      })
//    }
  }
}

object ChurchListTest extends App {
  import ChurchList._
}
