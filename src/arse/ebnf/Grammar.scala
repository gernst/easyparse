package arse.ebnf

import scala.language.postfixOps
import arse._

case class Grammar(rules: Map[Id, Expr]) {
  override def toString = {
    val lines = rules.map {
      case (id, expr) => id + " ::= " + expr + ";"
    }
    lines.mkString("", "\n", "\n")
  }
}

object Grammar {
  import Parser._
  import Recognizer._
  import Mixfix._

  def isLit(s: String) = {
    s.head == '\'' && s.last == '\''
  }

  object operators extends Syntax[String] {
    val prefix_ops: Map[String, Int] = Map()

    val postfix_ops: Map[String, Int] = Map(
      "*" -> 2,
      "+" -> 2)

    val infix_ops: Map[String, (Assoc, Int)] = Map(
      "|" -> (Right, 1))
  }

  def op(name: String) = name

  def ap(name: String, args: List[Expr]) = (name, args) match {
    case ("*", List(expr)) => Rep(expr, false)
    case ("+", List(expr)) => Rep(expr, true)
    case ("|", List(expr1, expr2)) => Alt(expr1, expr2)
  }

  val expr: Parser[String, Expr] = mixfix(closed, op, ap, operators)

  val keywords = Set("::=", ";", "(", ")")
  val tok = Tok.from(string filter isLit)
  val id = Id.from(string filterNot keywords)
  val seq = Seq.from(expr +)
  val closed = examples.parens(seq) | tok | id

  val lhs = id
  val rhs = seq

  val production = lhs ~ "::=" ~ rhs ~ ";"
  val productions = production *

  val grammar = productions map apply
  
  def apply(productions: List[(Id, Expr)]): Grammar = {
    val grouped = productions.groupBy(_._1)
    val rules = grouped.map {
      case (id, alts) =>
        (id, alts.map(_._2).reduceRight(Alt))
    }
    Grammar(rules)
  }
}