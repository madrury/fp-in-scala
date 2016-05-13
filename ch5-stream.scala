package fpinscala

sealed trait Stream[+A] {

    // 5.1: Consume a stream and make a List of its contents.
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
    }

    // 5.2: take the first n elements of a stream
    // Assumes that n >= 0
    def unsafeTake(n: Int): Stream[A] = {
        if (n == 0) return Stream.empty
        else return this match {
            case Empty => Stream.empty
            case Cons(h, t) => Stream.cons(h(), t().unsafeTake(n - 1)) 
        }
    }
    def take(n: Int): Option[Stream[A]] = {
        if (n < 0) {return None}
        else Some(this.unsafeTake(n))
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
        lazy val head = h
        lazy val tail = t
        return Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }


}
