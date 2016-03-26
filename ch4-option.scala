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
import Option._


// Ex 4.2
def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}
def variance(xs: Seq[Double]): Option[Double] = {
    // Inside a flat map, you can pretend the value is defined
    // You dont need to create a Seq[Option[Double]], you just short circit if
    // the mean does not exist
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
// println(mean(List(1, 2, 3, 4)))
// println(variance(List(1, 2, 3, 4)))


// Exercise 4.1
val x:Option[Int] = Some(1)
val y:Option[Int] = Some(2)
val n:Option[Int] = None

// println(x.map((t:Int) => t + 1))
// println(x.map((t:Int) => 5 * t))
// println(n.map((t:Int) => 5 * t))
// println(x.flatMap((t: Int) => Some(t)))
// println(n.flatMap((t: Int) => Some(t)))
// println(x.getOrElse(2))
// println(n.getOrElse(2))
// println(x.orElse(Some(2)))
// println(n.orElse(Some(2)))
// println(x.filter((t: Int) => t == 0))
// println(x.filter((t: Int) => t == 1))
// println(n.filter((t: Int) => t == 0))

// Exercise 4.3
//println(map2_2(x, y)((x, y) => x + y))
//println(map2_2(n, y)((x, y) => x + y))
//println(map2_2(n, n)((x, y) => x + y))

// Exercise 4.4
println(sequence_2(List(Some(1), Some(2), Some(3))))
println(sequence_2(List(Some(1), None:Option[Int], Some(3))))

// Exercise 4.5
println(traverse_2(List(1, 2, 3))(x => Some(2*x)))
println(traverse_2(List(List(1.0, 2.0, 3.0), List(2.0)))(mean))
println(traverse_2(List(List(1.0, 2.0, 3.0), List()))(mean))
