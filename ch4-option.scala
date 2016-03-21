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
import Option._


// Exercise 4.1
val x:Option[Int] = Some(1)
val n:Option[Int] = None

//println(x.map((t:Int) => t + 1))
//println(x.map((t:Int) => 5 * t))
//println(n.map((t:Int) => 5 * t))
//
//println(x.flatMap((t: Int) => Some(t)))
//println(n.flatMap((t: Int) => Some(t)))
//
//println(x.getOrElse(2))
//println(n.getOrElse(2))

// println(x.orElse(Some(2)))
// println(n.orElse(Some(2)))

println(x.filter((t: Int) => t == 0))
println(x.filter((t: Int) => t == 1))
println(n.filter((t: Int) => t == 0))
