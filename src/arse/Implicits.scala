// ARSE Parser libary
// (c) 2015 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import control._

trait Primitives {
  this: Combinators =>

  implicit def string = __ map { _.toString }

  // implicit def boolean = lit_]("true", true) | lit_]("false", false)
  implicit def boolean = string map { _.toBoolean.mask[IllegalArgumentException] }
  implicit def int = string map { _.toInt.mask[NumberFormatException] }
  implicit def long = string map { _.toLong.mask[NumberFormatException] }
  implicit def float = string map { _.toFloat.mask[NumberFormatException] }
  implicit def double = string map { _.toDouble.mask[NumberFormatException] }
  implicit def bigint = string map { BigInt(_).mask[NumberFormatException] }
}

trait Collections {
  this: Combinators =>

  implicit def option[A](implicit p: Parser[T, A]) = p ?
  implicit def list[A](implicit p: Parser[T, A]) = p *
}

trait Punctuation {
  this: Combinators =>

  def lparen = lit("(")
  def rparen = lit(")")
  def lbrack = lit("[")
  def rbrack = lit("]")
  def lbrace = lit("{")
  def rbrace = lit("}")
  def dot = lit(".")
  def colon = lit(":")
  def semicolon = lit(";")
  def equals = lit("=")
  def pipe = lit("|")
}
