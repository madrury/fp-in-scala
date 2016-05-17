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
    // 5.7: filter
    def filter(p: A => Boolean): Stream[A] =
        foldRight(Stream.empty[A])((a, s) => {
            if (p(a)) Stream.cons(a, s)
            else s 
        })
    // 5.7: append, add elements of one stream to the end of another
    def append[B >: A](z: => Stream[B]): Stream[B] =
        foldRight(z)(Stream.cons(_, _))
    // 5.7: flatMap
    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(Stream.empty[B])((a, b) => f(a).append(b))

    def find(p: A => Boolean): Option[A] = this.filter(p(_)).headOption

    // 5.13: map in terms of unfold.
    def mapFromUnfold[B](f: A => B): Stream[B] =
        Stream.unfold(this)(s => s match {
            case Cons(a, b) => Some((f(a()), b()))
            case Empty => None
        })
    // take in terms of unfold
    def takeFromUnfold(n: Int): Stream[A] =
        Stream.unfold((this, n))(s => s match { 
            case (stream, 0) => None
            case (stream, k) => stream match {
                case Cons(a, b) => Some((a(), (b(), k - 1)))
                case Empty => None
            }
        })
    // takeWhile in terms of unfold
    def takeWhileFromUnfold(p: A => Boolean): Stream[A] = 
        Stream.unfold(this)(s => s match {
            case Cons(a, b) => if (p(a())) Some((a(), b())) else None
            case Empty => None
        })
    // zipWith in terms of unfold
    def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
        Stream.unfold((this, that))(s => s match {
            case (Cons(a, arest), Cons(b, brest))
                => Some( (f(a(), b()), (arest(), brest())) )
            case _ => None
        })

    def zip[B](that: Stream[B]): Stream[(A, B)] =
        this.zipWith(that)((a, b) => (a, b))

    // Zips together two streams, even after one of them becomes exhausted.
    def zipWithAll[B, C](that: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
        Stream.unfold((this, that))(s => s match {
            case (Cons(a, arest), Cons(b, brest))
                => Some(( f(Some(a()), Some(b())), (arest(), brest()) ))
            case (Cons(a, arest), Empty)
                => Some(( f(Some(a()), None:Option[B]), (arest(), Stream.empty[B]) ))
            case (Empty, Cons(b, brest))
                => Some(( f(None:Option[A], Some(b())), (Stream.empty[A], brest()) ))
            case (Empty, Empty)
                => None
        })

    def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
        this.zipWithAll(that)((a, b) => (a, b))

    // 5.14: Implement startsWith
    def startsWith[A](a: Stream[A]): Boolean =
        this.zipAll(a).takeWhile(!_._2.isNone).map(
            s => s match {
                case (Some(a), Some(b)) => a == b
                case _ => false
            }
        ).all(_ == true)
        // This implementation fails by erroniously returning true when the
        // haystack stream gets exhausted before the needle
        //this.zipWith(a)(_ == _).all(_ == true)

    // 5.15: Implement tails
    def tails: Stream[Stream[A]] =
        Stream.unfold(this)(s => s match {
            case Cons(a, arest) => Some((s, arest()))
            case Empty => None
        })

        
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

    // 5.8: Infinite stream yielding a constant
    def ones: Stream[Int] = Stream.cons(1, Stream.ones)
    def constant[A](a: A): Stream[A] = Stream.cons(a, Stream.constant(a))

    // 5.9: Generate an infinite sequence of integers n, n+1, n+2, ...
    def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n+1))

    // 5.10: Generate the fibonnaci sequence
    def fibsFrom(i: Int, j: Int): Stream[Int] =
        Stream.cons(i, Stream.fibsFrom(j, i + j))
    def fibs: Stream[Int] = Stream.fibsFrom(0, 1)

    // 5.11: A general corecursive function for generating a stream
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z) match {
            case Some((a, s)) => Stream.cons(a, Stream.unfold(s)(f))
            case _ => Stream.empty[A]
        }
    }

    // 5.12 constant, from and fibs using unfold.
    def constantFromUnfold[A](a: A): Stream[A] =
        Stream.unfold(a)(s => Some((a, a)))
    def fromFromUnfold(n: Int): Stream[Int] =
        Stream.unfold(n)(n => Some((n, n + 1)))
    def fibFromUnfold: Stream[Int] =
        Stream.unfold((0, 1))(s => s match {
            case (n0, n1) => Some((n0, (n1, n0 + n1)))
            case _ => None  // State is always a tuple, so this should never happen
        })

}
