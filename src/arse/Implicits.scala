// ARSE Parser libary
// (c) 2015 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import control._

trait Primitives {
  this: Combinators =>

  implicit val string = __ map { _.toString }

  // implicit val boolean = lit("true", true) | lit("false", false)
  implicit val boolean = string map { _.toBoolean.mask[IllegalArgumentException] }
  implicit val int = string map { _.toInt.mask[NumberFormatException] }
  implicit val long = string map { _.toLong.mask[NumberFormatException] }
  implicit val float = string map { _.toFloat.mask[NumberFormatException] }
  implicit val double = string map { _.toDouble.mask[NumberFormatException] }
  implicit val bigint = string map { BigInt(_).mask[NumberFormatException] }
}

trait Collections {
  this: Combinators =>

  implicit def option[A](implicit p: Parser[T, A]) = p.?
  implicit def list[A](implicit p: Parser[T, A]) = p.*
}

trait Punctuation {
  this: Combinators =>

  val lparen = lit("(")
  val rparen = lit(")")
  val lbrack = lit("[")
  val rbrack = lit("]")
  val lbrace = lit("{")
  val rbrace = lit("}")
  val dot = lit(".")
  val colon = lit(":")
  val semicolon = lit(";")
  val equals = lit("=")
  val pipe = lit("|")
}
