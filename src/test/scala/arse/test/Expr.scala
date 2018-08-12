package arse.test

import scala.language.postfixOps
import arse._
import arse.implicits._

trait Expr
case class Id(name: String) extends Expr
case class App(args: List[Expr]) extends Expr

object Expr {
  val name = S("[a-zA-Z]+")
  val expr: Parser[Expr] = P(app | id)
  val id = Id(name)
  val app = App("(" ~ (expr *) ~ ")")
  
  implicit val W = Whitespace.default
  
  def main(args: Array[String]) {
    val in = "(a b (c d) e)"
    val out = expr.parse(in)
    println(out)
  }
}