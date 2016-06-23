package arse

package object examples {
  import Recognizer._

  def expect(s: String) = s ! "expected " + s

  def parens[E](p: Parser[String, E]) = {
    "(" ~ p ~ expect(")")
  }
}