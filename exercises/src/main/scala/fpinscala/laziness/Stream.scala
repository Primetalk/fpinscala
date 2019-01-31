package fpinscala.laziness

import Stream._
sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) =>
      val a = h()
      if (f(a)) Some(a)
      else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n  > 1 =>
      lazy val tt = t().take(n - 1)
      Cons(h, () => tt)
    case Cons(h, _) if n == 1 => Cons(h, () => Empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n >= 1 => t().drop(n - 1)
    case _                    => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      val hh = h()
      if(p(hh))
        Cons(() => hh, () => t().takeWhile(p))
      else
        Empty
  }

  def dropWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => t().dropWhile(p)
    case _ => this
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  def forAll2(p: A => Boolean): Boolean = {
    val notP: A => Boolean = a => !p(a)
    !exists(notP)
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, s) => cons(f(a), s))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, s) => if(p(a)) cons(a, s) else s)

  def append[B >: A](other: Stream[B]): Stream[B] =
    foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])(f(_).append(_))
//    foldRight(Empty: Stream[B])((a, s) => f(a).append(s))

  def zipWith[B](s: Stream[B]): Stream[(A, B)] = (this, s) match {
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(() => (h1(), h2()), () => t1().zipWith(t2()))
  }

  def startsWith[B](s: Stream[B]): Boolean = zipWith(s).forAll{ case (a, b) => a == b }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None =>
        Empty
      case Some((a, s)) =>
        cons(a, unfold(s)(f))
    }
}