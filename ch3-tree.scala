sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    // Ex 2.25
    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(left, right) => Tree.size(left) + Tree.size(right)
    }

    // Ex 2.26
    def maximum(tree: Tree[Int]): Int = tree match {
        case Leaf(a) => a
        case Branch(left, right) => Tree.maximum(left) max Tree.maximum(right)
    }

    // Ex 2.27
    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 0
        case Branch(left, right) => (Tree.depth(left) max Tree.depth(right)) + 1
    }

    // Ex 2.28
    def map[A, B](tree: Tree[A], f: A => B): Tree[B] =  tree match {
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) => Branch(Tree.map(left, f), Tree.map(right, f))
    }

    // Ex 2.29
    def fold[A, B, C](tree: Tree[A])(f: A => B, g: (B, B) => B): B = tree match {
        case Leaf(a) => f(a)
        case Branch(left, right) => g(Tree.fold(left)(f, g), Tree.fold(right)(f, g))
    }
    def sizeWithFold[A](tree: Tree[A]): Int = {
        Tree.fold(tree)((a: A) => 1, (z1: Int, z2: Int) => z1 + z2)
    }
    def maximumWithFold(tree: Tree[Int]): Int = {
        Tree.fold(tree)((a: Int) => a, (z1: Int, z2:Int) => z1 max z2)
    }
    def depthWithFold[A](tree: Tree[A]): Int = {
        Tree.fold(tree)((a: A) => 0, (z1: Int, z2: Int) => z1 max z2 + 1)
    }
    def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
        Tree.fold(tree)((a: A) => Leaf(f(a)), 
                        (left: Tree[B], right: Tree[B]) => Branch(left, right))
    }

}

import Tree._

val t:Tree[Int] = Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3))) 
val t2:Tree[Int] = Branch(
    Branch(Leaf(0), Leaf(1)),
    Branch(Leaf(2), Branch(
        Leaf(3), Leaf(4)))
) 

// Ex 2.25
//println(Tree.size(t))
//println(Tree.size(t2))

// Ex 2.26
//println(Tree.maximum(t))

// Ex 2.27
//println(Tree.depth(t))
//println(Tree.depth(t2))

// Ex 2.28
//println(Tree.map(t, (x: Int) => 2 * x))
//println(Tree.map(t2, (x: Int) => 2 * x))

// Ex 2.29
println(Tree.sizeWithFold(t))
println(Tree.sizeWithFold(t2))
println(Tree.maximumWithFold(t))
println(Tree.depthWithFold(t))
println(Tree.depthWithFold(t2))
println(Tree.mapWithFold(t)((x: Int) => 2 * x))
println(Tree.mapWithFold(t2)((x: Int) => 2 * x))
