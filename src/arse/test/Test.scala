package arse.test

import scala.language.postfixOps

object Test extends tst.Test {
  def main(args: Array[String]) {
    test("simple") {
      import arse.simple._

      val s = List("x", "x", "x")
      val p = tok("x")
      val r = tok("z")
      val q = p.+ | r
      
      val as = q(s)
      println(as)

      // as expect s
    }

    test("lisp") {
      import arse.examples.Lisp._

      val parse = expr.$

      val cases = List(
        (List("x"), Id("x")),
        (List("(", "x", ")"), App(List(Id("x")))))

      for ((in, out) <- cases) {
        parse(in) expect out
      }
    }
  }
}