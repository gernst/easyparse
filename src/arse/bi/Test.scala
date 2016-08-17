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
  
  val expr: Rel[Expr, List[String]] = rec(id | app)

  val id = Id.from(string).to[Expr]
  val app = App.from(string, expr +).to[Expr]
  
  // val expr = rec(app | id)
  // val id = Id ! (string)
  // val app = App.from(string, expr +)

  def main(args: Array[String]) {
    val s = List("x", "y")
    println(s)
    val a = expr parse s
    println(a)
    val b = expr format a
    println(b)
  }
}