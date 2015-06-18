package arse

import control._
import Punctuation._
import Combinators._

sealed trait Expr

case class Id(name: String) extends Expr {
  if (name == "(" || name == ")") fail
  override def toString = name
}

case class Num(n: BigInt) extends Expr {
  override def toString = n.toString
}

case class App(args: Seq[Expr]) extends Expr {
  override def toString = args.mkString("(", " ", ")")
}

object Base extends Combinators with Primitives {
  type T = String
  val id = parse(Id)
  val num = parse(Num)
}

object Lisp extends Combinators with Primitives with Collections with Punctuation {
  type T = String

  import Base._

  def _App(lp: LParen, args: Seq[Expr], rp: RParen): App = App(args)

  implicit val expr_r = rec(expr)
  implicit val exprs = rec(expr *)

  val app = parse(_App _)
  val expr: Parser[String, Expr] = app | num | id
}

object Test {
  val input = List("(", "a", "1", "(", "c", ")", "(", ")", ")")
  //val input = List("a")

  def main(args: Array[String]): Unit = {
    // val (res, _) = Lisp.expr(input)
    // println(res)
  }
}