package arse.bi

import scala.language.postfixOps

import arse._

object Test {
  type Parser = Rel[Expr, List[String]]

  sealed trait Expr

  case class Id(name: String) extends Expr
  case class App(fun: String, args: List[Expr]) extends Expr

  object _Id extends InvFunction1(Id.apply, Id.unapply)
  object _App extends InvFunction2(App.apply, App.unapply)

  val expr: Parser = rec(app | id)
  val id: Parser = _Id.from(__)
  val app: Parser = _App.from(__[String], expr +)

  def main(args: Array[String]) {
    val s = List("x", "y")
    println(s)
    val a = expr apply s
    println(a)
    val b = expr unapply a
    println(b)
  }
}