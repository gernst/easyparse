// ARSE Parser libary
// (c) 2015 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import punctuation._
import control._

sealed trait Expr

case class Id(name: String) extends Expr {
  if (name == "(" || name == ")") fail
  override def toString = name
}

case class Num(n: BigInt) extends Expr {
  override def toString = n.toString
}

case class App(args: List[Expr]) extends Expr {
  override def toString = args.mkString("(", " ", ")")
}

object Lisp extends Combinators with Primitive with Punctuation with Collections {
  type Token = String

  implicit val expr_r = rec(expr)
  // implicit val exprs = rec(expr *)

  val id = parse(Id)
  val num = parse(Num)
  val app = lit("(") ~> parse(App) <~ lit(")")
  val expr: Parser[Expr] = app | num | id
}

object Test {
  val input = List("(", "a", "1", "(", "c", ")", "(", ")", ")")
  //val input = List("a")

  def main(args: Array[String]): Unit = {
    val (res, _) = Lisp.expr(input)
    println(res)
  }
}
