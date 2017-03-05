package arse

import arse.ll._

package object examples {
  import Recognizer._

  def expect(s: String) = s ! "expected " + s

  def parens[E](p: Parser[List[String], E]) = {
    "(" ~ p ~ expect(")")
  }
}