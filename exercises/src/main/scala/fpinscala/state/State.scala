package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) = {
    val (l, rng) = nextLong
    val n = (l >>> 16).toInt
    (n, rng)
  }

  def nextLong: (Long, RNG) // Should generate a random `Long`. We'll later define other functions in terms of `nextLong`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  def apply(seed: Long): RNG = Simple(seed)

  case class Simple(seed: Long) extends RNG {
    def nextLong: (Long, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      (newSeed, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def flatMap0[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
    s.andThen{ case (a, r) => f(a)(r) }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case r@(i, rng2) =>
      if(i >= 0) r
      else nonNegativeInt(rng2)
  }

  def double(rng: RNG): (Double, RNG) = rng.nextInt match {
    case r@(i, rng2) =>
      if(i == Int.MinValue) double(rng2)
      else (i.toDouble / Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = int(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = int(rng2)
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if(count == 0)
      (Nil, rng)
    else
      flatMap0(int)(i => ints(count - 1): Rand[List[Int]])(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap0(ra)(a => map(rb)(b => f(a,b)))

  def map_[A,B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap0(ra)(f.andThen(unit[B]))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, bs) => flatMap(bs)(b => map(f)(ff => ff :: b)))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = flatMap0(f)(g)
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for{
      a <- this
      b <- sb
    } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = f(a).run(s2)
      (b, s3)
    })

  def withFilter(p: A => Boolean): State[S, A] =
    flatMap(a => if(p(a)) State.unit(a) else throw new IllegalStateException(s"$a failed the predicate p"))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def step(i: Input): State[Machine, Int] = i match {
      case Coin => State{ case Machine(_, candies, coins) =>
        (0, Machine(locked = candies == 0, candies, coins + 1))
      }
      case Turn => State{
        case Machine(false, candies, coins) if candies > 0 =>
          (1, Machine(locked = true, candies - 1, coins))
        case m =>
          (0, m)
      }
    }

    val dispensed: State[Machine, Int] = traverse(inputs)(step).map(_.sum)
    for {
      _ <- dispensed
      Machine(_, candies, coins) <- get[Machine]
    } yield (candies, coins)
  }

//  def set[S,A](s: State[S, A]): State[S, S] = State(s => (s, s))
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))


  def get[S]: State[S, S] =
    State(s => (s, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](List[A]()))((bs, f) => bs.flatMap(b => f.map(ff => ff :: b)))

  def traverse[S, A, B](list: List[A])(f: A => State[S, B]): State[S, List[B]] =
    sequence(list.map(f))
}
