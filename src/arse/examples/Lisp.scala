package arse.examples

import scala.language.postfixOps
import arse._

object Lisp {
  sealed trait Expr
  case class Id(name: String) extends Expr
  case class App(args: List[Expr]) extends Expr

  import Parser._

  val keywords = Set("(", ")")
  val name = string filterNot keywords
  
  val expr: Parser[List[String], Expr] = Parser.rec(top)
  val id = Id.from(name)
  val app = App.from(expr *)
  val top = parens(app) | id
}