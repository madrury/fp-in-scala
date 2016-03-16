/* Find the index of the first element in an array satisfying a predicate. */
def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
        if (n >= as.length) -1
        else if (p(as(n))) n
        else loop(n + 1)
    }
    loop(0)
}

// println(findFirst(Array(0, 1, 2, 3, 4), (x: Int) => x == 3))
// println(findFirst(Array(0, 1, 2, 3, 4), (x: Int) => x % 2 == 1))


/* Check if an array A is sorted with respoect to a binary predicate */
def isSorted[A](as: Array[A], p: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
        if (n >= as.length) true
        else if (!p(as(n-1), as(n))) false
        else loop(n + 1)
    }
    loop(1)
}

// println(isSorted(Array(0, 1, 2, 3, 4), (x: Int , y: Int) => x < y))
// println(isSorted(Array(0, 1, 3, 2, 4), (x: Int , y: Int) => x < y))


/* Curry a function */
def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    def g(a: A): B => C = {
        def h(b: B): C = {
            f(a, b)
        }
        h
    }
    g
}

// println(curry((x: Int , y: Int) => x + y)(1)(2))
// println(curry((x: Int , y: Int) => x + y)(2)(2))


/* Uncurry a function */
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    def g(a: A, b: B): C = {
        f(a)(b)
    }
    g
}

// println(uncurry((x: Int) => ((y: Int) => x + y))(1, 1)) 
// println(uncurry((x: Int) => ((y: Int) => x + y))(1, 2)) 


/* Function composition */
def compose[A, B, C](f: B => C, g: A => B): A => C = {
    def h(a: A): C = {
        f(g(a))
    }
    h
}

println(compose((x: Int) => 2 * x, (x: Int) => 2 * x)(1))
println(compose((x: Int) => 2 * x, (x: Int) => 2 * x)(2))
