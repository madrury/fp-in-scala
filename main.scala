package fpinscala

object App {

    def main(args: Array[String]): Unit = {


        // Chapter 4: Option exercises.
        //--------------------------------------------------------------------
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
        // val x:Option[Int] = Some(1)
        // val y:Option[Int] = Some(2)
        // val n:Option[Int] = None

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
        // println(sequence_2(List(Some(1), Some(2), Some(3))))
        // println(sequence_2(List(Some(1), None:Option[Int], Some(3))))

        // Exercise 4.5
        // println(traverse_2(List(1, 2, 3))(x => Some(2*x)))
        // println(traverse_2(List(List(1.0, 2.0, 3.0), List(2.0)))(mean))
        // println(traverse_2(List(List(1.0, 2.0, 3.0), List()))(mean))


        // Chapter 5: Stream exercises.
        //--------------------------------------------------------------------
        val x = Stream(1, 2, 3, 4)
        val y = Stream(0, 0, 0, 0)
        val z = Stream(1, 1, 1, 1, 1, 1)
        //println(x.toList)
        //println(x.takeFromUnfold(2).toList)
        //println(x.drop(2).map(_.toList))
        //println(x.takeWhileFromUnfold(_ < 3).toList)
        //println(x.takeWhileFoldRight(_ < 3).toList)
        //println(x.any(_ > 4))
        //println(x.all(_ < 2))
        //println(x.any(_ == 3))
        //println(x.all(_ > 0))
        // println(x.headOption)
        // println(Stream.empty.headOption)

//        println(x.mapFromUnfold(2 * _).toList)
//        println(x.filter(_ % 2 == 0).toList)
//        println(x.append(x).toList)
//        println(x.flatMap(t => Stream(t, t)).toList)
        //println(Stream.ones.take(5).map(_.toList))
        //println(Stream.ones.takeWhile(_ == 1).toList) // Blows stack
        //println(Stream.constantFromUnfold(2).take(5).map(_.toList))
        //println(Stream.fromFromUnfold(2).take(5).map(_.toList))
        //println(Stream.fibFromUnfold.take(20).map(_.toList))
        println(x.zipWith(y)(_ + _).toList)
        println(x.zipWith(Stream.constant(1))(_ + _).toList)
        println(x.zip(y).toList)
        println(x.zipAll(z).toList)

    }

}
