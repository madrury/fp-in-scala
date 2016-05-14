package fpinscala

sealed trait Stream[+A] {

    // 5.1: Consume a stream and make a List of its contents.
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
    }

    // 5.2: take the first n elements of a stream
    // unsafeTake assumes that the interger argument is non-negative, it will
    //   fail otherwise.
    // take wraps unsafeTake and returns an option.  It fails if the integer
    //   argument is non-negative.
    def unsafeTake(n: Int): Stream[A] = {
        if (n == 0) Stream.empty
        else this match {
            case Empty => Stream.empty
            case Cons(h, t) => Stream.cons(h(), t().unsafeTake(n - 1)) 
        }
    }
    def take(n: Int): Option[Stream[A]] = {
        if (n < 0) None
        else Some(this.unsafeTake(n))
    }

    // 5.2: drop the first n arguments of a string.
    def unsafeDrop(n: Int): Stream[A] = {
        if (n == 0) this
        else this match {
            case Empty => Stream.empty
            case Cons(h, t) => t().unsafeDrop(n - 1)
        }
    }
    def drop(n: Int): Option[Stream[A]] = {
        if (n < 0) None
        else Some(this.unsafeDrop(n)) 
    }

    // 5.3 take the largest initial segment of a sequence for which all
    //   elements satisfy a predicate.
    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Empty => Stream.empty
        case Cons(h, t) => {
            if (p(h())) Stream.cons(h(), t().takeWhile(p))
            else Stream.empty
        }
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
