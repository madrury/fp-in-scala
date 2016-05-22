package fpinscala

trait RNG {
    def nextInt: (Int, RNG)
}

// Base linear random number generator.
case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }

}


object Random {

    // Type alias for random number generating state machine.
    type Rand[+A] = RNG => (A, RNG)

    // Pass through the state uchanged, and generate a constant value.
    def unit[A](a: A): Rand[A] = rng => (a, rng)

    // Transform the output of a random number generator using a function.
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng1) = s(rng)
            (f(a), rng1)
        }

    // 6.6: Transform the output of a pair of random number generators.
    def map2[A, B, C](ar: Rand[A], br: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, rng1) = ar(rng)
            val (b, rng2) = br(rng1)
            (f(a, b), rng2)
        }
    }

    def both[A, B](ar: Rand[A], br: Rand[B]): Rand[(A, B)] =
        map2(ar, br)((_, _))

    // 6.7: Transform a list of random A's to a random list of A's
    def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = {
        rng => {
            // Folding over a (Rand[A], (RNG, List[A])) => (RNG, List[A])
            rs.foldRight((List[A](), rng)) {
                case (ra, (as, rng0)) => {
                    val (a, rng1) = ra(rng0)
                    (a :: as, rng1)
                }
            }
        }
    }

    // The solution to to this problem in the solutions manual.
    // Ingenious (at least to a newbie).  It folds a
    //     (Rand[A], Rand[List[A]]) => Rand[List[A]]
    // using map2 to tread the rng state through the list concatinations.
    def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
        fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

    
    // 6.8
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
        rng => {
            val (a, rng1) = f(rng)
            g(a)(rng1)
        }

    def nonNegativeLessThan(n: Int): Rand[Int] =
        flatMap(nonNegativeInt)(i => { 
            val mod = i % n
            if (i + (n - 1) - mod >= 0) unit(mod)
            else nonNegativeLessThan(n)
        })


    val integer: Rand[Int] = _.nextInt

    // 6.1: Implement nonNegativeInt
    // Int.MinValue is one less than -Int.MaxValue
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (n, st) = rng.nextInt
        val k = if (n < 0) -(n + 1) else n
        (k, st)
    }

    def nonNegativeEven: Rand[Int] =
        Random.map(nonNegativeInt)(i => i - i % 2)

    // 6.2: Generate a random double.
    def double(rng: RNG): (Double, RNG) = {
        val (n, st) = Random.nonNegativeInt(rng)
        val k = n.toDouble / Int.MaxValue
        (k, st)
    }
    // 6.5: Implement double with map
    def doubleWithMap: Rand[Double] =
        Random.map(integer)(i => i.toDouble / Int.MaxValue)

    // 6.3: Generate tuples of random numbers.
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, rng1) = Random.integer(rng)
        val (f, rng2) = Random.double(rng1)
        ((i, f), rng2)
    }
    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val ((i, f), rng1) = Random.intDouble(rng)
        ((f, i), rng1)
    }
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (f1, rng1) = Random.double(rng)
        val (f2, rng2) = Random.double(rng1)
        val (f3, rng3) = Random.double(rng2)
        ((f1, f2, f3), rng3)
    }

    // 6.4: Generate a list of random integers of a given size.
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def go(k: Int, acc: List[Int], r: RNG): (List[Int], RNG) = {
            if(k == 0) (acc, r)
            else {
                val (n, r1) = Random.integer(r)
                go(k - 1, n :: acc, r1)
            }
        }
        go(count, List(), rng)
    }

}
