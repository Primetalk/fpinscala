/**
  * Copyright (C) 2015 Cotiviti Labs (nexgen.admin@cotiviti.io)
  *
  * The software code contained herein is the property of Cotiviti Corporation
  * and its subsidiaries and affiliates (collectively, “Cotiviti”).
  * Access to this software code is being provided to you in the course of your
  * employment or affiliation with Cotiviti and may be used solely in the scope
  * and course of your work for Cotiviti, and is for internal Cotiviti use only.
  * StreamBasedParser unauthorized use, disclosure, copying, distribution, destruction of this
  * software code, or the taking of any unauthorized action in reliance on this
  * software code, is strictly prohibited.
  * If this information is viewed in error, immediately discontinue use of the
  * application.  StreamBasedParserone using this software code and the applications will be
  * subject to monitoring for improper use, system maintenance and security
  * purposes, and is advised that if such monitoring reveals possible criminal
  * activity or policy violation, Cotiviti personnel may provide the evidence of
  * such monitoring to law enforcement or other officials, and the user may be
  * subject to disciplinary action by Cotiviti, up to and including termination
  * of employment.
  *
  * Use of this software code and any applications and information therein
  * constitutes acknowledgement of and consent to this notice
  */
package fpinscala.parsing

import cats.FlatMap
import fs2._

object StreamParserImplHelper {
  type StreamParseResult[+A] = Either[StreamParseError, A]
  type CharStream = Stream[Pure, Char]

  type StreamParseError = String

  type StreamBasedParser[+A] =
    CharStream => Stream[StreamParseResult, A]

  implicit object StreamParseResultFlatMap extends FlatMap[StreamParseResult] {
    override def flatMap[A, B](fa: StreamParseResult[A])(f: A => StreamParseResult[B]): StreamParseResult[B] = ???

    override def tailRecM[A, B](a: A)(f: A => StreamParseResult[Either[A, B]]): StreamParseResult[B] = ???

    override def map[A, B](fa: StreamParseResult[A])(f: A => B): StreamParseResult[B] = ???
  }
}

import StreamParserImplHelper._

object StreamParserImpl extends Parsers[StreamBasedParser] {

  override def or[A](s1: StreamBasedParser[A], s2: => StreamBasedParser[A]): StreamBasedParser[A] = ???

  override def fail(message: String): StreamBasedParser[Nothing] = ???

  override implicit def string(s: String): StreamBasedParser[String] = ???
//    input =>
//      input.

  override implicit def char(c: Char): StreamBasedParser[Char] =
    input =>
      input.evalMap(ch =>
        if(ch == c)
          Stream.emit[StreamParseResult, Char](c)
        else
          Stream.emit(Left(c.toString))
      )

  override implicit def anyChar: StreamBasedParser[Char] =
    _.flatMap(ch =>
      Stream.emit(Right(ch))
    )

  override def charPredicate(f: Char => Boolean): StreamBasedParser[Char] = ???

  override def flatMap[A, B](p: StreamBasedParser[A])(f: A => StreamBasedParser[B]): StreamBasedParser[B] = ???

  override def slice[A](p: StreamBasedParser[A]): StreamBasedParser[String] = ???

  override def run[A](value: StreamBasedParser[A])(string: String): StreamParserImpl.ParseResult[A] = ???

  override def location(e: ParseError): Location = ???

  override def message(e: ParseError): String = ???
}
