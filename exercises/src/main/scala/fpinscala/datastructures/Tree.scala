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

}