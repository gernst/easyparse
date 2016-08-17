package arse.bi

import scala.language.postfixOps

import arse._

object Test {
  import Seq._
  
  sealed trait Expr
  case class Id(name: String) extends Expr
  case class App(fun: String, args: List[Expr]) extends Expr

  object Expr extends Rec(App | Id, "Expr")
  object Id extends Rel1[String, Id, Expr, List[String]](string)
  object App extends Rel2[String, List[Expr], App, Expr, List[String]](string, Expr +)
  
  case class Foo(a: String, b: String)
  object Foo extends Iso2[String, String, Foo]
  
  val p = Foo.from(string, string)
  
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