package fpinscala.testing

import java.util.concurrent.atomic.AtomicReference

import fpinscala.state._
import Prop._

import scala.collection.mutable
import scala.util.Try

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

//sealed trait Prop {
//  def check: Boolean
//  def &&(other: => Prop): Prop = new Prop {
//    override def check: Boolean = check && other.check
//  }
//}
case class Prop(run: (MaxSize, TestCases) => State[RNG, Result]) {
  def &&(other: => Prop): Prop = Prop(
    (maxSize: MaxSize, testCases: TestCases) =>
      for {
        r1 <- run(maxSize, testCases)
        r2 <- other.run(maxSize, testCases)
      } yield (r1, r2) match {
        case (Passed, Passed) => Passed
        case (Proved, Passed) => Passed
        case (Passed, Proved) => Passed
        case (Proved, Proved) => Proved
        case (Falsified(cnt1, msg1), Falsified(cnt2, msg2)) =>
          Falsified(cnt1 + cnt2, msg1 + msg2)
        case (f@Falsified(cnt1, msg1), _) => f
        case (_, f@Falsified(cnt1, msg1)) => f
      }

  )

}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  sealed trait Result
  case object Proved extends Result
  case object Passed extends Result
  case class Falsified(passCount: Int, message: String) extends Result
//  type Result = Option[(Int, String)]
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    forAll(gen.unsized)(f)

  def forAll[A](sgen: SGen[A])(f: A => Boolean): Prop = Prop(
    (maxSize: MaxSize, testCases: TestCases) => State[RNG, Result]{rng =>
      def loop(cnt: Int, rng: RNG): (Result, RNG) =
        if(cnt == 0)
          (Passed, rng)
        else {
          val (a, rng2: RNG) = sgen.forSize(maxSize).sample.run(rng)
          Try(f(a)) match {
            case scala.util.Success(true) =>
              loop(cnt - 1, rng2)
            case scala.util.Success(false) =>
              (Falsified(testCases - cnt, "property is not true for " + a), rng2)
            case scala.util.Failure(exception) =>
              (Falsified(testCases - cnt, "failed with exception: " + exception.getMessage + " on argument " + a), rng2)
          }

        }
      loop(testCases, rng)
  }
  )

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)
  ): Unit =
    p.run(maxSize, testCases).run(rng)._1 match {
      case Falsified(n, msg) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

  def prove(pred: => Boolean): Prop =
    Prop((_, _) => if(pred) State.unit(Proved) else State.unit(Falsified(1, "property is false")))
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen[A](State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State{rng =>
    val (i, rng2) = rng.nextInt
    (i % (stopExclusive - start) + start, rng2)
  })

  def double(start: Double, stopExclusive: Double): Gen[Double] = {
    val k: Double = (stopExclusive - start) / (Int.MaxValue.toDouble - Int.MinValue)
    val b: Double = start - k * Int.MinValue

    Gen(State{rng =>
      val (i, rng2) = rng.nextInt
      val x = i.toDouble
      (k * x + b, rng2)
    })
  }

  def boolean: Gen[Boolean] = Gen(State{rng =>
    val (i, rng2) = rng.nextInt
    ((i / 4) % 2 == 1, rng2)
  })

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    Gen(State{ rng =>
      def loop(i: Int, rng: RNG, res: List[A]): (List[A], RNG) = {
        if(i == 0)
          (res, rng)
        else {
          val (v, rng2) = gen.sample.run(rng)
          loop(i - 1, rng2, v :: res)
        }
      }

      require(n >= 0, "Cannot generate list of negative length")
      loop(n, rng, Nil)
  })

  def listOf1N[A](n: Int, gen: Gen[A]): Gen[List[A]] = {
    //    gen.flatMap(head => listOfN(n - 1, gen).map(head :: _))
    for {
      head <- gen
      tail <- listOfN(math.max(0, n - 1), gen)
    } yield head :: tail
  }

  def listOfN[A](nGen: Gen[Int])(gen: Gen[A]): Gen[List[A]] =
    nGen
      .flatMap(n => listOfN(n, gen))

  def oneOf[A](gens: Gen[A]*): Gen[A] =
    choose(0, gens.size)
      .flatMap(gens)

  def weighted[A](gens: (Gen[A], Double)*): Gen[A] = {
    case class Range(start: Double, finish: Double, gen: Gen[A]) {
      def contains(x: Double): Boolean = x >= start && x < finish
    }
    val (total: Double, ranges: List[Range]) = gens.foldLeft((0.0, List[Range]())){
      case ((l, lst), (gen, weight)) =>
        val nextL = l + weight
        (nextL, Range(l, nextL, gen) :: lst)
    }

    double(0, total)
      .flatMap(x =>
        ranges
          .find(_.contains(x))
          .map(_.gen)
          .getOrElse(throw new IllegalArgumentException(s"Couldn't find generator for $x"))
      )
  }

  def memoize[A, B](f: A => B): A => B = {
    val map = mutable.Map[A, B]()// todo concurrent hash map

    a => map.getOrElseUpdate(a, f(a))
  }

  def funBad[A, B](gen: Gen[B]): Gen[A => B] = {
    Gen(State[RNG, A => B] { rng =>
      val (seed, rng2) = rng.nextInt
      val random: AtomicReference[RNG] = new AtomicReference(RNG(seed.toLong)) // a new RNG for function values.
      val f = memoize[A, B] { _ =>
        val rng3 = random.get()
        val (b, rng4: RNG) = gen.sample.run(rng3)
        random.compareAndSet(rng3, rng4)
        b
      }
      (f, rng2) // we are returning the next random generator
      // so that if we use it for generating a function
      // it'll be another function.
    })
  }

  /**
    * Correct functional implementation of generator of arbitrary functions.
    *
    * It uses `Cogen` to rewind random number generator to a position that depends on both
    * the initial RNG position and provided value `A`. Then this new RNG is used to generate `B`.
    *
    * It has the following properties:
    * 1. For the same RNG state identical functions are generated.
    * 2. All functions are robust and deterministic.
    * 3. Different initial RNG positions yield different functions.
    */
  def fun[A, B](cogen: Cogen[A], gen: Gen[B]): Gen[A => B] = {
    Gen(State[RNG, A => B] { rng =>
      /** This function captures the state of rng and then reuse it
        * on each call.
        */
      def f(a: A): B = {
        val rng2 = cogen.rewind(a, rng)
        val (b, _) = gen.sample.run(rng2)
        b
      }
      (f, rng.nextInt._2)
    })
  }

}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f.andThen(_.sample)))

  def unsized: SGen[A] = SGen[A](_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(forSize andThen(_.map(f)))
//    SGen(size => forSize(size).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(size => forSize(size).flatMap(f(_).forSize(size)))

  def flatMapIncorrect[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize andThen(_.flatMap(f)))
//    SGen(size => forSize(size).flatMap(f))

}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(size => Gen.listOfN(size, g))
}

/** Rewinds RNG to a new position using A. */
case class Cogen[A](rewind: (A, RNG) => RNG) {
  def contramap[B](f: B => A): Cogen[B] =
    Cogen{ (b, rng0) => rewind(f(b), rng0) }
}

object Cogen {
  def long: Cogen[Long] =
    Cogen((l, rng) => RNG.apply(l ^ rng.nextLong._1))

  def int: Cogen[Int] =
    long.contramap(_.toLong)
}
