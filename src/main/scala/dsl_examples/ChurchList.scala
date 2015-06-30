package dsl_examples

object ChurchList {
  sealed trait ChurchList[+T] {
    def fold[K]: K => (T => K => K) => K

    def toList: List[T] = fold[List[T]](Nil)(x => xs => x::xs)
    def isEmpty: Boolean = this.fold(true)(_ => _ => false)
    def ++[X >: T](xs: ChurchList[X]): ChurchList[X] = fold[ChurchList[X]](xs)(y => ys => Cons(y, ys))

    override def toString = s"ChurchList(${this.toList.mkString(",")})"
  }

  object ChurchList {
    def apply[T](l: T*): ChurchList[T] = l.toList match {
      case Nil => Empty
      case x::xs => new ChurchList[T] {
        val rest = ChurchList[T](xs: _*)
        def fold[K]: K => (T => K => K) => K =
          nil => cons => cons(x)(rest.fold(nil)(cons))
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
        nil => cons => cons(x)(xs.fold(nil)(cons))
    }

    def unapply[T](l: ChurchList[T]): Option[(T, ChurchList[T])] = {
      l.fold[Option[(T, ChurchList[T])]](None)(h => {
        case Some((x, xs)) => Some((h, Cons(x, xs)))
        case None => Some((h, Empty))
      })
    }
  }
}

object ChurchListTest extends App {
  import ChurchList.{ChurchList => CL, _}

  CL(1,2,3) match {
    case Cons(x, xs) => {
      assert(x == 1, "head")
      assert(xs.toList == List(2, 3), "tail")
      println("success 1")
    }
  }

  // if you try this:
  //   CL(1,2,3,4) match { case Cons(x1, Cons(x2, rest)) => (x1, x2, rest) }
  // in the REPL, it will crash the compiler: bug?
  CL(1,2,3,4) match {
    case Cons(x1, Cons(x2, rest)) => {
      assert(x1 == 1, "car")
      assert(x2 == 2, "cadr")
      assert(rest.toList == List(3, 4), "cddr")
      println("success 2")
    }
  }
}
