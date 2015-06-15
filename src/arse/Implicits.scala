package arse

import control._

trait Primitive {
  this: Combinators =>

  implicit val string = __ ^^ { _.toString }
  
  // implicit val boolean = lit("true", true) | lit("false", false)
  implicit val boolean = string ^^ { _.toBoolean.mask[IllegalArgumentException] }
  implicit val int = string ^^ { _.toInt.mask[NumberFormatException] }
  implicit val long = string ^^ { _.toLong.mask[NumberFormatException] }
  implicit val float = string ^^ { _.toFloat.mask[NumberFormatException] }
  implicit val double = string ^^ { _.toDouble.mask[NumberFormatException] }
  implicit val bigint = string ^^ { BigInt(_).mask[NumberFormatException] }
}

trait Collections {
  this: Combinators =>

  implicit def option[A](implicit p: Parser[A]) = p ?
  implicit def list[A](implicit p: Parser[A]) = p *
}

object punctuation {
  case class LParen()
  case class RParen()
  case class Dot()
  case class Colon()
  case class Semicolon()
}

trait Punctuation {
  this: Combinators =>

  import punctuation._
  
  implicit val lparen = lit("(", LParen())
  implicit val rparen = lit(")", RParen())
  implicit val dot = lit(".", Dot())
  implicit val colon = lit(":", Colon())
  implicit val semicolon = lit(";", Semicolon())

}