object FactorialModule {

    def factorial(n: Int): Int = {
        def go(n: Int, acc: Int): Int = {
            if (n <= 0) acc
            else go(n - 1, n * acc)
        }
        go(n, 1)
    }

    private def formalFactorial(n: Int): String = {
        val msg = "The factorial of %d is %d"
        msg.format(n, factorial(n))
    }

    def main(args: Array[String]): Unit = {
        println(formalFactorial(9))
    }

}
