package arse.bi

import scala.language.postfixOps

import arse._

object Test {
  import Seq._
  
  sealed trait Expr
  case class Id(name: String) extends Expr
  case class App(fun: String, args: List[Expr]) extends Expr

  object Expr extends Rec(App | Id, "Expr")
  object Id extends Rel1[String, Id, Expr, List[String]](string) with Iso1[String, Id]
  object App extends Rel2[String, List[Expr], App, Expr, List[String]](string, Expr +) with Iso2[String, List[Expr], App]
  
  val expr: Rel[Expr, List[String]] = rec(id | app)
  val id = Id.from(string)
  val app = App.from(string, expr +)
  
  // val expr = rec(app | id)
  // val id = Id ! (string)
  // val app = App.from(string, expr +)

  def main(args: Array[String]) {
    val s = List("x", "y")
    println(s)
    val a = Expr parse s
    println(a)
    val b = Expr format a
    println(b)
  }
}