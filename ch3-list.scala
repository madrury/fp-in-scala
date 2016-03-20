sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

    def sum(ints: MyList[Int]): Int = ints match {
       case Nil => 0
       case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: MyList[Double]): Double = ds match {
        case Nil => 1
        case Cons(x, xs) => x * product(xs)
    }
   
    // apply is a sepcial function that allows us to use the datatype name as
    // a constructor.  i.e. MyList(1, 2, 3, 4)
    def apply[A](as: A*): MyList[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

    // This throws a match not exahutive warning, which I don't know how to
    // deal with quite yet.
    def head[A](as: MyList[A]): A = as match {
        case Nil => sys.error("Attempt to take head of empty list")
        case Cons(x, xs) => x
    }


    // First exerceise set: General pattern matching
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

    // def 3.6
    def init[A](as: MyList[A]): MyList[A] = as match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, MyList.init(xs))
    }


    // Second exerceise set: Higher order functions

    // foldRight((1, 2, 3, 4), 0)(+) = (1 + (2 + (3 + (4 + 0))))
    def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
    
    // Ex 3.9
    def length[A](as: MyList[A]): Int = {
        MyList.foldRight(as, 0)((a, b) => b + 1)
    }

    // Ex 3.10
    // foldRight is not tail recursive.  This tail recursive version is foldLeft.
    // foldLeft((1, 2), 0)(+)
    //     = foldLeft((2,), (0 + 1))(+) 
    //     = foldLeft((,), ((0 + 1) + 2))(+)
    //     = ((0 + 1) + 2)
    def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    // Ex 3.12
    // The idea in the accumulator implementation is to pass elements from the
    // arugment list onto the LHS of the accumulator.
    def reverse[A](as: MyList[A]): MyList[A] = {
        def loop[A](as: MyList[A], acc: MyList[A]): MyList[A] = as match {
            case Nil => acc
            case Cons(x, xs) => loop(xs, Cons(x, acc))  
        }
        loop(as, MyList())
    }
    // This is essentially the same as the previous implementation, as the
    // following expansion shows
    //   reverseFoldLeft((1, 2, 3))
    //     = foldLeft((1, 2, 3), (,))(...)
    //     = foldLeft((2, 3), Cons(1, (,))(...)
    //     = foldLeft((3,), Cons(2, Cons(1, (,))))(...)
    //     = foldLeft((,), Cons(3, Cons(2, Cons(1, (,)))))(...)
    ///    = Cons(3, Cons(2, Cons(1, (,))))
    def reverseFoldLeft[A](as: MyList[A]): MyList[A] = {
        MyList.foldLeft(as, MyList[A]())((l, a) => Cons(a, l))
    }

    // Ex 3.13
    def foldRightInTermsOfLeft[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = {
        MyList.foldLeft(MyList.reverse(as), z)((b, a) => f(a, b))
    }

    // Ex 3.14
    def append[A](a1s: MyList[A], a2s: MyList[A]): MyList[A] = {
        MyList.foldRight(a1s, a2s)(Cons(_, _))
    }

    // Ex 3.15
    def coalesce[A](aas: MyList[MyList[A]]): MyList[A] = {
        MyList.foldRight(aas, Nil:MyList[A])(MyList.append(_, _))
    }

    // Ex 3.18
    def map[A, B](as: MyList[A], f: A => B): MyList[B] = {
        MyList.foldRight(as, Nil:MyList[B])((a, l) => Cons(f(a), l))
    }
        
    // Ex 3.19
    def filter[A](as: MyList[A], p: A => Boolean): MyList[A] = as match {
        case Nil => Nil
        case Cons(x, xs) => if (p(x)) Cons(x, filter(xs, p)) else filter(xs, p)
    }

    // Ex 3.20
    def flatMap[A, B](as: MyList[A], f: A => MyList[B]): MyList[B] = {
        MyList.foldRight(as, Nil:MyList[B])(
            (a: A, bs: MyList[B]) => MyList.append(f(a), bs)
        )
    }
    // A possibly more transparant way is to map and then coalesce
    def flatMap2[A, B](as: MyList[A], f: A => MyList[B]): MyList[B] = {
        MyList.coalesce(MyList.map(as, f))
    }

    // Ex 3.21
    def filterWithFlatMap[A](as: MyList[A], p: A => Boolean): MyList[A] = {
        MyList.flatMap(as, (a: A) => if (p(a)) MyList(a) else Nil:MyList[A])        
    }

    // Ex 3.22, 3.23
    // We don't have a mixed type collection yet, so zipping together two
    // List's of like data-type will have to do for now.
    def zip[A](a1s: MyList[A], a2s: MyList[A]): MyList[MyList[A]] = (a1s, a2s) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(x, xs), Cons(y, ys)) => Cons(MyList(x, y), MyList.zip(xs, ys))
    }
    def zipWith[A, B](a1s: MyList[A], a2s: MyList[A], f: (A, A) => B): MyList[B] = {
        MyList.map(MyList.zip(a1s, a2s), (as: MyList[A]) => as match {
            case Cons(a1, Cons(a2, Nil)) => f(a1, a2)
            case _ => sys.error("Non ordered pair found.")
        })
    }

    // Ex 3.24
    def isEmpty[A](as: MyList[A]): Boolean = as match {
        case Nil => true
        case _ => false
    }
    def beginsWith[A](as: MyList[A], stub: MyList[A]): Boolean = {
        if (MyList.isEmpty(stub)) true
        else if (MyList.isEmpty(as)) false
        else if (MyList.head(as) != MyList.head(stub)) false
        else MyList.beginsWith(MyList.tail(as), MyList.tail(stub))
    }
    def hasSubsequence[A](as: MyList[A], sub: MyList[A]): Boolean = as match {
        case Nil => false
        case Cons(x, xs) => MyList.beginsWith(as, sub) || MyList.beginsWith(xs, sub)
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

// Ex 3.6
// println(MyList.init(x))

// Ex 3.8: Using foldRight to build up the origional list
//println(MyList.foldRight(MyList(1, 2, 3, 4), Nil:MyList[Int])(Cons(_, _)))

// Ex 3.9
// println(MyList.length(MyList()))
// println(MyList.length(MyList(1, 2, 3, 4)))

// Ex 3.10
// println(MyList.foldLeft(MyList(1, 2, 3, 4), 0)(_ + _))
// println(MyList.foldLeft(MyList(1, 2, 3, 4), 1)(_ * _))
//println(MyList.foldLeft(MyList(1, 2, 3, 4), Nil:MyList[Int])((x, y) => Cons(y, x)))

// Ex 3.12
// println(MyList.reverse(MyList(1, 2, 3, 4)))
// println(MyList.reverseFoldLeft(MyList(1, 2, 3, 4)))

// Ex 3.13
// println(MyList.foldRightInTermsOfLeft(MyList(1, 2, 3, 4), 0)(_ + _))
// println(MyList.foldRightInTermsOfLeft(MyList(1, 2, 3, 4), Nil:MyList[Int])(Cons(_, _)))

// Ex 3.14
// println(MyList.append(MyList(1, 2, 3), MyList(4, 5, 6)))

// Ex 3.15
// println(MyList.coalesce(MyList(MyList(1, 2), MyList(3, 4), MyList(5, 6))))

// Ex 3.18
// println(MyList.map(MyList(1, 2, 3, 4), (a: Int) => a + 1))
// println(MyList.map(MyList(1, 2, 3, 4), (a: Int) => 0))

// Ex 3.19
// println(MyList.filter(x, (x: Int) => x % 2 == 0))
// println(MyList.filter(x, (x: Int) => x % 2 == 1))

// Ex 3.20
// println(MyList.flatMap(x, (x: Int) => MyList(x, x)))
// println(MyList.flatMap2(x, (x: Int) => MyList(x, x)))

// Ex 3.21
//  println(MyList.filterWithFlatMap(x, (x: Int) => x % 2 == 0))

// Ex 3.22
// println(MyList.zipWith(MyList(1, 2, 3), MyList(1, 2, 3), (x: Int, y: Int) => x + y))

// Ex 2.24
println(MyList.beginsWith(MyList(1, 2, 3, 4), MyList(1, 2)))
println(MyList.beginsWith(MyList(1, 2, 3, 4), MyList(2, 3)))
println(MyList.beginsWith(MyList(1, 2), MyList(1, 2, 3)))
println(MyList.hasSubsequence(MyList(1, 2, 3, 4), MyList(2, 3)))
println(MyList.hasSubsequence(MyList(1, 2, 3, 4), MyList(2, 4)))
