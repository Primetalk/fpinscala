package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }


  def foldLeft[A, B](tree: Tree[A], z: B)(f: (B, A) => B): B = tree match {
    case Leaf(a) => f(z, a)
    case Branch(left, right) =>
      val zLeft = foldLeft(left, z)(f)
      foldLeft(right, zLeft)(f)
  }

  // Write a function `size` that counts the number of nodes (leaves
  // and branches) in a tree.
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  // Write a function `maximum` that returns the maximum element in a
  // `Tree[Int]`. (Note: In Scala, you can use `x.max(y)` or `x max y`
  // to compute the maximum of two integers `x` and `y`.)
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(left, right) => math.max(maximum(left), maximum(right))
  }


  // Write a function `depth` that returns the maximum path length
  // from the root of a tree to any leaf.
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => math.max(depth(left), depth(right)) + 1
  }

  // Write a function `map`, analogous to the method of the same name
  // on List, that modifies each element in a tree with a given
  // function.
  def map_[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Generalize `size`, `maximum`, `depth`, and `map`, writing a new
  // function `fold` that abstracts over their similarities. Reimplement
  // them in terms of this more general function. Can you draw an
  // analogy between this fold function and the left and right folds for
  // List?
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def const[A, B](a: A)(ignore: B): A = a
  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(const(1))(_ + _)
  def maximumViaFold[A](t: Tree[Int]): Int = fold(t)(identity)(_ max _)
  def depthViaFold[A](t: Tree[A]): Int = fold(t)(const(1))(_ max _ + 1)
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
