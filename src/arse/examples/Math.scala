package arse.examples

import scala.language.postfixOps
import arse._

object Math {
  sealed trait Expr
  case class Num(n: Double) extends Expr
  case class App(fun: String, args: List[Expr]) extends Expr

  import Parser._
  import Mixfix._

  def id[A](a: A) = a

  val expr: Parser[String, Expr] = mixfix(top, id, App, Operators)
  val num = Num.from(double)
  val args = parens(expr *)
  val app = App.from(string, args)
  val top = parens(expr) | app | num

  def fac(n: Double): Double = {
    if(n <= 0) 1
    else fac(n - 1) * n
  }

  def eval(e: Expr): Double = e match {
    case Num(n)                 => n
    case App("-", List(e))      => -eval(e)
    case App("!", List(e))      => fac(eval(e))
    case App("+", List(e1, e2)) => eval(e1) + eval(e2)
    case App("-", List(e1, e2)) => eval(e1) - eval(e2)
    case App("*", List(e1, e2)) => eval(e1) * eval(e2)
    case App("/", List(e1, e2)) => eval(e1) / eval(e2)
  }
}

object Operators extends Syntax[String] {
  val prefix_ops = Map(
    "-" -> 10)

  val postfix_ops = Map(
    "!" -> 11) // factorial

  val infix_ops = Map(
    "*" -> (Left, 9),
    "/" -> (Left, 9),
    "+" -> (Left, 8),
    "-" -> (Left, 8))
}
