// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse.test

import scala.language.postfixOps

import arse.P
import arse.Parser
import arse.Whitespace
import arse.input
import arse.int
import arse.toLit

object Calc extends tst.Test {
  val expr: Parser[Int] = P(sum)
  val expr_eof = expr $

  val literal = P("(" ~ expr ~ ")" | int)
  val product = P(literal ~+ "*" map { _.product })
  val sum = P(product ~+ "+" map { _.sum })

  implicit object W extends Whitespace("\\s*")

  def parse(text: String) = {
    expr_eof parse text
  }

  test("literal") {
    parse("").fails

    parse("a").fails
    parse("1a").fails

    parse("0") expect 0
    parse("1") expect 1

    for (i <- -Short.MinValue to Short.MaxValue) {
      parse(i.toString) expect i
    }
  }

  test("sum") {
    parse("0+").fails
    parse("0") expect 0
    parse("0+0") expect 0
    parse("0+0+0") expect 0
    parse("0+0+0+0") expect 0

    parse("0+1") expect 1
    parse("1+0") expect 1
    parse("1+1") expect 2
    parse("12+34+56") expect (12 + 34 + 56)
  }

  test("product") {
    parse("0") expect 0
    parse("0*0") expect 0
    parse("0*0*0") expect 0
    parse("0*0*0*0") expect 0

    parse("0*1") expect 0
    parse("1*0") expect 0
    parse("1*1") expect 1
    parse("12*34*56") expect (12 * 34 * 56)
  }

  test("expr") {
    parse("(0)") expect 0
    parse("1+2*3") expect (1 + 2 * 3)
    parse("1*2+3") expect (1 * 2 + 3)
    parse("(1+2)*3") expect ((1 + 2) * 3)
    parse("1*(2+3)") expect (1 * (2 + 3))
  }

  def main(args: Array[String]) {
  }
}