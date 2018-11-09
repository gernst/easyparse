package arse.test

import arse._
import arse.implicits._

object Snippets {
   val p = (int ?~ "a") | (int ?~ "b" ?~ "c") | (int ?~ "b" ?~ "d")
   
   def main(args: Array[String]) {
     implicit val w = Whitespace.default
     println(p parse "0bd")
   }
}