/* Find the index of the first element in an array satisfying a predicate. */
def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
        if (n >= as.length) -1
        else if (p(as(n))) n
        else loop(n + 1)
    }
    loop(0)
}

println(findFirst(Array(0, 1, 2, 3, 4), (x: Int) => x == 3))
println(findFirst(Array(0, 1, 2, 3, 4), (x: Int) => x % 2 == 1))


/* Check if an array A is sorted with respoect to a binary predicate */
def isSorted[A](as: Array[A], p: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
        if (n >= as.length) true
        else if (!p(as(n-1), as(n))) false
        else loop(n + 1)
    }
    loop(1)
}

println(isSorted(Array(0, 1, 2, 3, 4), (x: Int , y: Int) => x < y))
println(isSorted(Array(0, 1, 3, 2, 4), (x: Int , y: Int) => x < y))
