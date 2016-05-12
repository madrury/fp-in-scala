sealed trait Stream[+A] {

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
import Stream._

val x = Stream(1, 2, 3, 4)
println(x.toList)
