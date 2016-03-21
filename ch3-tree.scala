sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(left, right) => Tree.size(left) + Tree.size(right)
    }

}

import Tree._

val t:Tree[Int] = Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3))) 

// Ex 2.25
println(Tree.size(t))
