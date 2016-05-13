package fpinscala

sealed trait Stream[+A] {

    // 5.1: Consume a stream and make a List of its contents.
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
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
