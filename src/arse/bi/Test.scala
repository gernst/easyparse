package arse.bi

import scala.language.postfixOps

import arse._

object Test {
  import Seq._

  sealed trait Expr
  case class Id(name: String) extends Expr
  case class App(fun: String, args: List[Expr]) extends Expr

  object Id extends Iso1[String, Id]
  object App extends Iso2[String, List[Expr], App]

  val keywords = Set("(", ")")
  val name = string filterNot keywords

  val expr: Rec[Expr, List[String]] = rec(app | id)
  val id = Id.from(name)
  val app = App.from(name, "(" ~ expr.+ ~ ")")

  val top = expr ~ $

  def main(args: Array[String]) {
    val s = List("x", "(", "y", "z", "(", "a", ")", ")")
    println(s)
    val a = top parse s
    println(a)
    val b = top format a
    println(b)
  }
}