package fpinscala

sealed trait Stream[+A] {

    // 5.1: Consume a stream and make a List of its contents.
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
    }

    // Because of the => B as the second argument to f, the second argument's
    // evaluation is deferred until it's value is needed.  The first argument
    // (the head of the strema in the recursive call) is evaluated when that
    // call is put on the stack.
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Empty => z
        // The head is evaluated right here, but the recursive call is
        // delayed.  If f can return only based on the value of h, then the
        // recursive call is never evaluated.
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }

    // This is passed as the last argument?  Good idea.
    def any(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)
    def all(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

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

    // 5.3: take the largest initial segment of a sequence for which all
    //   elements satisfy a predicate.
    // 5.5: implement takeWhile using foldRight.
    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Empty => Stream.empty
        case Cons(h, t) => {
            if (p(h())) Stream.cons(h(), t().takeWhile(p))
            else Stream.empty
        }
    }
    def takeWhileFoldRight(p: A => Boolean): Stream[A] =
        this.foldRight(Stream.empty[A])((a, b) => {
            if (p(a)) Stream.cons(a, b)
            else Stream.empty
        })

    // 5.6 headOption using foldRight
    def headOption: Option[A] =
        // Because the function being folded does not depend on its second
        // argument, the second argument never gets evaluated.
        foldRight(None: Option[A])((a, b) => Some(a))

    // 5.7: map
    def map[B](f: A => B): Stream[B] =
        foldRight(Stream.empty[B])((a, s) => Stream.cons(f(a), s))
    // 5.7 filter
    def filter(p: A => Boolean): Stream[A] =
        foldRight(Stream.empty[A])((a, s) => {
            if (p(a)) Stream.cons(a, s)
            else s 
        })
    // 5.7 append, add elements of one stream to the end of another
    def append[B >: A](z: => Stream[B]): Stream[B] =
        foldRight(z)(Stream.cons(_, _))
    // 5.7 flatMap
    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(Stream.empty[B])((a, b) => f(a).append(b))
        

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
