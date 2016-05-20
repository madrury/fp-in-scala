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

    // Int.MinValue is one less than -Int.MaxValue
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (n, st) = rng.nextInt
        val k = if (n < 0) -(n + 1) else n
        (k, st)
    }

    def double(rng: RNG): (Double, RNG) = {
        val (n, st) = Random.nonNegativeInt(rng)
        val k = n.toDouble / Int.MaxValue
        (k, st)
    }

}
