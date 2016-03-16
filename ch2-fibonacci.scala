object FibonacciModule {

    def fibonacci(n: Int): Int = {
         def go(n: Int, acc: Int, pacc: Int): Int = {
             if (n <= 1) acc
             else go(n - 1, acc + pacc, acc)
         }
         go(n, 0, 1)
    }

    private def formatFibonacci(n: Int): String = {
        val msg = "The %d th fibonacci number is %d"
        msg.format(n, fibonacci(n))
    }

    def main(args: Array[String]): Unit = {
        println(formatFibonacci(1))
        println(formatFibonacci(2))
        println(formatFibonacci(3))
        println(formatFibonacci(4))
        println(formatFibonacci(5))
        println(formatFibonacci(20))
    }

}
