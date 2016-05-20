package fpinscala

trait RNG {
    def nextInt: (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }

}

object Random {

    def integer(rng: RNG): (Int, RNG) = rng.nextInt
  
    // 6.1: Implement nonNegativeInt
    // Int.MinValue is one less than -Int.MaxValue
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (n, st) = rng.nextInt
        val k = if (n < 0) -(n + 1) else n
        (k, st)
    }

    // 6.2: Generate a random double.
    def double(rng: RNG): (Double, RNG) = {
        val (n, st) = Random.nonNegativeInt(rng)
        val k = n.toDouble / Int.MaxValue
        (k, st)
    }

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
