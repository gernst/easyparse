package arse

import control._

trait Primitives {
  this: Combinators =>

  implicit def string = __ ^^ { _.toString }

  // implicit def boolean = lit_]("true", true) | lit_]("false", false)
  implicit def boolean = string ^^ { _.toBoolean.mask[IllegalArgumentException] }
  implicit def int = string ^^ { _.toInt.mask[NumberFormatException] }
  implicit def long = string ^^ { _.toLong.mask[NumberFormatException] }
  implicit def float = string ^^ { _.toFloat.mask[NumberFormatException] }
  implicit def double = string ^^ { _.toDouble.mask[NumberFormatException] }
  implicit def bigint = string ^^ { BigInt(_).mask[NumberFormatException] }
}

trait Collections {
  this: Combinators =>
  implicit def option[A](implicit p: Parser[T, A]) = p ?
  implicit def seq[A](implicit p: Parser[T, A]) = p *
}

object Punctuation {
  case class LParen()
  case class RParen()
  case class LBrack()
  case class RBrack()
  case class LBrace()
  case class RBrace()
  case class Dot()
  case class Colon()
  case class Semicolon()
  case class Equals()

}

trait Punctuation {
  this: Combinators =>

  import Punctuation._

  implicit def lparen = lit("(", LParen())
  implicit def rparen = lit(")", RParen())
  implicit def lbrack = lit("[", LBrack())
  implicit def rbrack = lit("]", RBrack())
  implicit def lbrace = lit("{", LBrace())
  implicit def rbrace = lit("}", RBrace())
  implicit def dot = lit(".", Dot())
  implicit def colon = lit(":", Colon())
  implicit def semicolon = lit(";", Semicolon())
  implicit def equals = lit("=", Equals())

}