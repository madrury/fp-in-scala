sealed trait Either[+E, +A] {

    // Ex 4.7
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => Right(f(a))
    }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


import Either._

val x:Either[String, Int] = Right(1)
val e:Either[String, Int] = Left("Error!")

// Ex 4.6
println(x.map((x:Int) => 2 * x))
println(e.map((x:Int) => 2 * x))
