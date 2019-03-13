package fpinscala.parsing

import language.{higherKinds, implicitConversions}

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  implicit def toParserOps[A](p: Parser[A]): ParserOps[A] =
    ParserOps(p)

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)
  }

  object Laws {
  }

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]

  def map[A,B](p: Parser[A])(f: A => B): Parser[B]

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2).map{ case (a,b) => f(a,b) }

  def many[A](p: Parser[A]): Parser[List[A]]

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 =>
  }

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