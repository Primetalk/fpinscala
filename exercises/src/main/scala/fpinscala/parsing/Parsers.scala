package fpinscala.parsing

import fpinscala.testing.{Gen, Prop}
import Prop.forAll
import language.{higherKinds, implicitConversions}

trait Parsers[Err, Parser[+_]] { self => // so inner classes may call methods of trait


  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def succeed[A](value: A): Parser[A] = string("").map(_ => value)

  def unit[A](value: A): Parser[A] = succeed(value)

  def failure(message: String): Parser[Nothing]

  implicit def string(s: String): Parser[String]

  implicit def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  implicit def anyChar: Parser[Char]

  def charPredicate(f: Char => Boolean): Parser[Char]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def toParserOps[A](p: Parser[A]): ParserOps[A] =
    ParserOps(p)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)

    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def as[B](b: => B): Parser[B] = self.map(p)(_ => b)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def manyWithSep(sep: Parser[_]): Parser[List[A]] = self.manyWithSep(p, sep)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def ~[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def *>[B](p2: Parser[B]): Parser[B] = flatMap(_ => p2)

    def ~>[B](p2: Parser[B]): Parser[B] = flatMap(_ => p2)

    def <*[B](p2: Parser[B]): Parser[A] = flatMap(a => p2.map(_ => a))

    def <~[B](p2: Parser[B]): Parser[A] = flatMap(a => p2.map(_ => a))

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def listOfNLaw: Prop =
      Prop.prove(
        self.run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
      ) && Prop.prove(
        self.run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
      ) && Prop.prove(
        self.run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
      )

    def productLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      ???
  }

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => unit(f(a)))

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2).map{ case (a,b) => f(a,b) }

  def many[A](p: Parser[A]): Parser[List[A]] = many1(p) | succeed(Nil)
  /** a, a, a, a */
  def manyWithSep[A](p: Parser[A], sep: Parser[_]): Parser[List[A]] =
    (p ~ many(sep ~> p)).map{ case (h, t) => h :: t } | succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => self.succeed(Nil)
    case 1 => p.map(List(_))
    case _ if n > 1 =>
      product(p, listOfN(n - 1, p))
        .map{ case (h,t) => h :: t }
    case _ => self.failure(s"listOfN for negative n is undefined (n = $n)")
  }

  def slice[A](p: Parser[A]): Parser[String]

  type ParseResult[A] = Either[ParseError, A]

  def run[A](value: Parser[A])(string: String): ParseResult[A]
}

case class Location(input: String, offset: Int = 0) {

  private def start: String = input.slice(0, offset + 1)
  lazy val line: Int = start.count(_ == '\n') + 1
  lazy val col: Int  = offset + 1 - math.max(0, start.lastIndexOf('\n'))


  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location =
    copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) {
      input.lines.drop(line - 1).next
    } else {
      ""
    }
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}