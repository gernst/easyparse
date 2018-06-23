package arse.test

import arse._
import arse.implicits._

object Snippets {
   val p = (int ~ "a") | (int ~ "b")
   
   def main(args: Array[String]) {
     implicit val w = Whitespace.default
     p parse "0b"
   }
}