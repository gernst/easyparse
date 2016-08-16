package arse.bi

import scala.language.postfixOps

import arse._

object Test {
  sealed trait Expr
  case class Id(name: String) extends Expr
  case class App(fun: String, args: List[Expr]) extends Expr

  object Expr extends Rec(App | Id, "Expr")
  object Id extends Rel1[String, Id, Expr, List[String]](string)
  object App extends Rel2[String, List[Expr], App, Expr, List[String]](string, Expr +)
  
  def main(args: Array[String]) {
    val s = List("x", "y")
    println(s)
    val a = Expr parse s
    println(a)
    val b = Expr format a
    println(b)
  }
}