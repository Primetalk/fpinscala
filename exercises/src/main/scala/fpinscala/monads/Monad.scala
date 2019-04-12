package fpinscala
package monads

import fpinscala.monoids.{Foldable, Monoid}
import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait WithUnit[M[_]] {
  def unit[A](a: => A): M[A]
}

trait WithZero[M[_]] {
  def zero[A]: M[A]
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = lma match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequence(t))((fh,tt) => fh :: tt)
  }

  def sequence2[A](lma: List[M[A]]): M[List[A]] =
     traverse(lma)(identity)

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def traverse2[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    sequence(la.map(f))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if(n <= 0)
      unit(List())
    else
      flatMap(ma)(a => map(replicateM(n - 1, ma))(la => a :: la))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
//    val lab: List[M[(A, Boolean)]] = ms.map(a => map(f(a))(flag => (a, flag)))
//    val ml: M[List[(A, Boolean)]] = sequence(lab)
//    map(ml)(list => list.filter(_._2).map(_._1))
    val mloa: M[List[Option[A]]] =
      traverse[A, Option[A]](ms){
        a => flatMap(f(a))(flag => if(flag) unit(Option(a)) else unit(None))
      }
    map(mloa)(_.flatten)
  }

  def filterM2[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
    case Nil => unit(List.empty[A])
    case a :: as => flatMap(f(a)) { flag =>
      if (flag) map(filterM(as)(f)) { a :: _ }
      else filterM(as)(f)
    }
  }

  def traverseGen[A, B, L[_], R[_]](la: L[A])(f: A => M[R[B]])(implicit ML: Foldable[L], MLR: Monoid[R[B]]): M[R[B]] =
    ML.foldRight(la)(unit(MLR.zero))((a: A, fbs: M[R[B]]) => map2(f(a), fbs)(MLR.op))

//  def traverseGen2[A, B, L[_], R[_]](la: L[A])(f: A => M[R[B]])(implicit ML: Monad[L], MLR: Monoid[R[B]]): M[R[B]] = {
//    val aa: L[M[R[B]]] = ML.map(la)(f)
//    //((a: A, fbs: M[R[B]]) => map2(f(a), fbs)(MLR.op))
//  }

  def filterMGen[A, L[_]](ms: L[A])(f: A => M[Boolean])(implicit ML: Foldable[L], MR: Monad[L], MM: Monoid[L[A]]): M[L[A]] = {
    traverseGen[A, A, L, L](ms){
      a => flatMap(f(a))(flag => if(flag) unit(MR.unit(a)) else unit(MM.zero))
    }
  }

//  def filterList[A](la: List[A])(f: A => M[Boolean]) =
//    filterMGen[A, List, List](la)(f)(ListFoldable, Monoid.listMonoid, Monad.listMonad)

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose[Unit, A, B](_ => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))



}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      ma flatMap f
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.unit(a)
    override def flatMap[A,B](ma: P[A])(f: A => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
      ma.flatMap(f)
  }


  trait StateT[S] {
    type Aux[A] = State[S, A]
  }

  def stateMonad[S]: Monad[StateT[S]#Aux] = new Monad[StateT[S]#Aux] {
    def unit[A](a: => A): StateT[S]#Aux[A] = State((a, _))
    override def flatMap[A,B](ma: StateT[S]#Aux[A])(f: A => StateT[S]#Aux[B]): StateT[S]#Aux[B] =
      State{ s =>
        val (a, s2) = ma.run(s)
        f(a).run(s2)
      }
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A,B](ma: Id[A])(f: A => Id[B]): Id[B] =
      ma.flatMap(f)
  }

  trait ReaderT[R] {
    type Aux[A] = Reader[R, A]
  }

  def readerMonad[R]: Monad[ReaderT[R]#Aux] = new Monad[ReaderT[R]#Aux] {
    def unit[A](a: => A): ReaderT[R]#Aux[A] = Reader(_ => a)
    override def flatMap[A,B](ma: ReaderT[R]#Aux[A])(f: A => ReaderT[R]#Aux[B]): ReaderT[R]#Aux[B] =
      Reader(r => f(ma.run(r)).run(r))
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R]: Monad[({ type f[x] = Reader[R, x]})#f] =
    new Monad[({type f[x] = Reader[R,x]})#f] {
      def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
      override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
        Reader(r => f(st.run(r)).run(r))
    }
}

