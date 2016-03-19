sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
   
    // apply is a sepcial function that allows us to use the datatype name as
    // a constructor.  i.e. MyList(1, 2, 3, 4)
    def apply[A](as: A*): MyList[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

    def head[A](as: MyList[A]): A = as match {
        // case Nil => Unit // Should really be an error 
        case Cons(x, xs) => x
    }

    // Ex 3.2
    def tail[A](as: MyList[A]): MyList[A] = as match {
        case Nil => Nil  // Should probably be an error
        case Cons(x, xs) => xs
    }

    // Ex 3.3
    def setHead[A](as: MyList[A], a: A): MyList[A] = as match {
        case Nil => Nil  // Should probably be an error
        case Cons(x, xs) => Cons(a, xs)
    }

    // Ex 3.4
    def drop[A](as: MyList[A], n: Int): MyList[A] = {
        def loop(as: MyList[A], n: Int): MyList[A] = {
            if (n == 0) as
            else loop(MyList.tail(as), n - 1) 
        }
        loop(as, n)
    }

    // Ex 3.5
    def dropWhile[A](as: MyList[A], p: A => Boolean): MyList[A] = {
        def loop(as: MyList[A]): MyList[A] = {
            if (!p(MyList.head(as))) as
            else loop(tail(as))
        }
        loop(as)
    }

    def sum(ints: MyList[Int]): Int = ints match {
       case Nil => 0
       case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: MyList[Double]): Double = ds match {
        case Nil => 1
        case Cons(x, xs) => x * product(xs)
    }

}

val x = MyList(1, 2, 3, 4)
// val y = MyList(1.0, 2.0, 3.0, 4.0)

// println(MyList.sum(x))

// println(MyList.product(y))

// Ex 3.2
// println(MyList.tail(x))

// Ex 3.3
// println(MyList.setHead(x, 5))

// Ex 3.4
//println(MyList.drop(x, 0))
//println(MyList.drop(x, 1))
//println(MyList.drop(x, 2))

// Ex 3.5
// println(MyList.dropWhile(x, (t: Int) => t <= 2))
