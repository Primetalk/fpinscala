package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  def monoid[A](zero: A)(op: (A, A) => A): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = op(a1, a2)
    val zero: A = zero
  }
  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero: Nil.type = Nil
  }

  val intAddition: Monoid[Int] = monoid(0)(_ + _)

  val intMultiplication: Monoid[Int] = monoid(1)(_ * _)

  val booleanOr: Monoid[Boolean] = monoid(false)(_ || _)

  val booleanAnd: Monoid[Boolean] = monoid(true)(_ && _)

  def optionMonoid[A](implicit M: Monoid[A]): Monoid[Option[A]] = monoid(None: Option[A]){
    case (Some(a), Some(b)) => Some(M.op(a,b))
    case (Some(a), _) => Some(a)
    case (_, Some(b)) => Some(b)
  }

  def endoMonoid[A]: Monoid[A => A] = monoid[A => A](identity)(_ compose _)

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  // trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(
    for{
      a <- gen
      b <- gen
      c <- gen
    } yield (a,b,c)
  ){ case (a,b,c) => m.op(a, m.op(b, c)) == m.op(m.op(a, b), c) }.&&(
    Prop.forAll(gen)(a => m.op(a, m.zero) == a)
  ).&&(
    Prop.forAll(gen)(a => m.op(m.zero, a) == a)
  )

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = // concatenate(as.map(f), m)
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val g: A => B => B = f.curried
    val fs: List[B => B] = as.map(g)
    fs.foldRight(z)(_ apply _)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val g: A => B => B = a => b => f(b,a)
    val fs: List[B => B] = as.map(g)
    fs.foldLeft(z){ case (b, gg) => gg(b) }
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match {
      case 0 => m.zero
      case 1 => f(as.head)
      case n =>
        val (left, right) = as.splitAt(n / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ints.isEmpty ||
      foldMapV( ints.zip(ints.tail), booleanOr){ case (a,b) => a <= b }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  import fpinscala.parallelism._
  import fpinscala.parallelism.Par.Par

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    monoid(fpinscala.parallelism.Par.unit(m.zero))(
      (pa, pb) =>
        Par.map2(pa, pb)(m.op)
    )

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.unit(f(a)))


  val wcMonoid: Monoid[WC] = {
    def cnt(str: String): Int = str.split(' ').length
    monoid[WC](Stub("")){
      case (Stub(l), Stub(r)) => Part(l, 0, r)
      case (Part(l, n, r), Stub(rr)) =>
        val rrr = r + rr
        val i = rrr.lastIndexOf(' ')
        if(i == -1)
          Part(l, n, rrr)
        else
          Part(l, n + cnt(rrr.substring(0, i)), rrr.substring(i + 1))
      case (Stub(ll), Part(l, n, r)) =>
        val lll = ll + l
        val i = lll.indexOf(' ')
        if(i == -1)
          Part(lll, n, r)
        else
          Part(lll.substring(0, i), n + cnt(lll.substring(i + 1)), r)
      case (Part(ll, n, r), Part(l, m, rr)) =>
        val c = r + l
        Part(ll, n + m + cnt(c), rr)
    }
  }

  def count(s: String): Int =
    ??? // foldMapV(s.toIndexedSeq, wcMonoid)(c => Stub(c.toString))

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = monoid((A.zero, B.zero)){
    case ((a1,b1),(a2,b2)) =>
      (A.op(a1, a2), B.op(b1, b2))
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    monoid[A => B](a => B.zero)((f1, f2) => a => B.op(f1(a), f2(a)))

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = monoid(Map[K, V]()){
    case (m1, m2) =>
      (m1.keySet ++ m2.keySet)
        .foldLeft(Map[K, V]())((m, k) =>
          m.updated(k,
            V.op(
              m1.getOrElse(k, V.zero),
              m2.getOrElse(k, V.zero)
            )
          )
        )
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}


trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    def flip[C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)
    foldLeft(as)(z)(flip(f))
  }

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ??? // foldMap(as)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).foldLeft(mb.zero)(mb.op)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.length match {
      case 0 => mb.zero
      case 1 => f(as.head)
      case n =>
        val (left, right) = as.splitAt(n / 2)
        mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }

}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(v) => f(v)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.fold(mb.zero)(f.andThen(mb.op(mb.zero, _)))
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
}

