/**
  * Copyright (C) 2015 Cotiviti Labs (nexgen.admin@cotiviti.io)
  *
  * The software code contained herein is the property of Cotiviti Corporation
  * and its subsidiaries and affiliates (collectively, “Cotiviti”).
  * Access to this software code is being provided to you in the course of your
  * employment or affiliation with Cotiviti and may be used solely in the scope
  * and course of your work for Cotiviti, and is for internal Cotiviti use only.
  * Any unauthorized use, disclosure, copying, distribution, destruction of this
  * software code, or the taking of any unauthorized action in reliance on this
  * software code, is strictly prohibited.
  * If this information is viewed in error, immediately discontinue use of the
  * application.  Anyone using this software code and the applications will be
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

import scala.language.higherKinds

sealed trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val whitespace: Parser[String] = (char(' ') | char('\n') | char('\r') | char('\t')).many.slice
    val quotedString: Parser[String] = char('"') ~> charPredicate(_ != '"').many.slice <~ char('"')
    val number: Parser[Double] = charPredicate("0123456789.".contains(_)).many.slice.map(_.toDouble)
    val jnull: Parser[JNull.type] = string("null").as(JNull)
    val jnumber: Parser[JNumber] = number.map(JNumber)
    val jbool: Parser[JBool] = (string("true").as(true)  | string("false").as(false)).map(JBool)
    val jstring: Parser[JString] = quotedString.map(JString)
    def jsonValue: Parser[JSON] =
      jbool | jnumber | jstring | jobject | jarray | jnull

    def labelValuePair: Parser[(String,JSON)] = (quotedString <~ whitespace <~ ":" <~ whitespace) ~ jsonValue

    def jobject: Parser[JObject] =
      ("{" ~> labelValuePair.manyWithSep(whitespace ~ "," ~ whitespace) <~ "}")
        .map(_.toMap)
        .map(JObject)

    def jarray: Parser[JArray] =
      ("[" ~> jsonValue.manyWithSep(whitespace ~ "," ~ whitespace) <~ "]")
        .map(_.toIndexedSeq)
        .map(JArray)
    jsonValue
  }
}