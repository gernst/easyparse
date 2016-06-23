package arse.examples

import arse._

object Lisp {
  sealed trait Expr
  case class Id(name: String) extends Expr
  case class App(args: List[Expr]) extends Expr

  import Parser._

  val expr: Parser[String, Expr] = Parser.rec(top)
  val id = Id.from(string)
  val app = App.from(expr *)
  val top = parens(app) | id
}