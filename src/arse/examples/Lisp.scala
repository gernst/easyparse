// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse.examples

import arse._
import java.util.Scanner

/* Minimal working example.
 * 
 *   expr = id | "(" expr ... expr ")" 
 */

sealed trait Expr

case class Id(name: String) extends Expr {
  override def toString = name
}

case class App(args: List[Expr]) extends Expr {
  override def toString = args.mkString("(", " ", ")")
}

object Parser extends Combinators with Primitives with Collections {
  import Combinators._

  type T = String

  /* recursive parser for expressions,
   * must be declared first so that subsequent productions can refer to it.
   */
  implicit val expr_r: Parser[String, Expr] = rec(expr)

  /* strict parser that fails if s cannot be matched */
  def expect(s: String) = lit(s) ! "expected " + s

  val keywords = Set("(", ")")
  val nonkw = string filterNot keywords

  // Productions
  implicit val id = parse(Id)(nonkw)
  implicit val app = lit("(") ~> parse(App) <~ expect(")")

  // Putting app first prevents id from consuming an open ( 
  val expr = app | id
}

object Lisp {
  // Trivial scanner that requires tokens to be surrounded by space
  def scan(line: String): List[String] = {
    line.split("\\s+").toList
  }

  // Parse a complete expression
  def parse(tokens: List[String]) = {
    val p = Parser.expr.$ // fail with leftover tokens
    p(tokens)
  }

  def main(args: Array[String]): Unit = {
    println(parse(scan("( x y z )")))
  }
}