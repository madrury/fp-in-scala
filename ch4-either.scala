sealed trait Either[+E, +A] {

    // Ex 4.6
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => b
        case Right(a) => Right(a)
    }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

//    def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] = {
//        a flatMap (aa => (b map (bb => f(aa, bb)))) 
//    }
    // With a for comprehension
    def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] = {
        for {
            aa <- a
            bb <- b
        } yield f(aa, bb)
    }
    
}

import Either._

val x:Either[String, Int] = Right(1)
val e:Either[String, Int] = Left("Error!")

// Ex 4.6
// println(x.map((x:Int) => 2 * x))
// println(e.map((x:Int) => 2 * x))

//println(x.flatMap((x:Int) => Right(2 * x)))
//println(x.flatMap((x:Int) => Left("Error!")))
//println(e.flatMap((x:Int) => Right(2*x)))
//println(e.flatMap((x:Int) => Left("Error!")))

//println(x.orElse(Right(1)))
//println(x.orElse(Left("Error!")))
//println(e.orElse(Right(1)))
//println(e.orElse(Left("Error!")))

val f = ((x:Int, y:Int) => x + y)
println(map2(x, x)(f))
println(map2(e, x)(f))
println(map2(x, e)(f))
println(map2(e, e)(f))
