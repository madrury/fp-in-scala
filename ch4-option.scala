package fpinscala

sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        // map: Option[A] => Option[Option[B]]
        // getOrElse => Option[Option[B]] => Option[B]
        this.map(f).getOrElse(None)
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        // map: Option[A] => Option[Option[A]]
        // getOrElse: Option[Option[A]] => Option[B]
        this.map(Some(_)).getOrElse(ob)
    }

    def filter(f: A => Boolean): Option[A] = {
        // map: Option[A] => Option[Boolean]
        if (this.map(f).getOrElse(false)) this else None
    }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

    // Ex 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(a), Some(b)) => Some(f(a, b))
    }
    // Implementation with no pattern matching, just higher order functions
    def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
        a flatMap (aa => b map (bb => f(aa, bb)))
    }


    // Ex 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
        // Option[A] => List[A]
        case Nil => Some(Nil)
        case None::xs => None
        case Some(a)::xs => map2(Some(a), sequence(xs))(_ :: _)
    }

    // Ex 4.5
    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
        case Nil => Some(Nil)
        case x::xs => map2(f(x), traverse(xs)(f))(_ :: _)
    }
    def traverse_2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
        as.foldRight(Some(Nil):Option[List[B]])((x, xs) => map2(f(x), xs)(_ :: _))
    }
    def sequence_2[A](a: List[Option[A]]): Option[List[A]] = {
        traverse(a)(x => x)
    }

}
