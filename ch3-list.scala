sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
   
    // apply is a sepcial function that allows us to use the datatype name as
    // a constructor.  i.e. List(1, 2, 3, 4)
    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

    def sum(ints: List[Int]): Int = ints match {
       case Nil => 0
       case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1
        case Cons(x, xs) => x * product(xs)
    }

}

