package arse.ebnf

import scala.language.postfixOps
import arse._

trait Expr
case class Tok(text: String) extends Expr {
  override def toString = text
}

case class Id(name: String) extends Expr {
  override def toString = name
}

case class Opt(expr: Expr) extends Expr {
  override def toString = expr + " ?"
}
case class Rep(expr: Expr, isPlus: Boolean) extends Expr {
  override def toString = {
    if(isPlus) expr + " +" else expr + " *"
  }
}
case class Alt(expr1: Expr, expr2: Expr) extends Expr {
  override def toString = expr1 + " | " + expr2
}

case class Seq(exprs: List[Expr]) extends Expr {
  override def toString = exprs match {
    case List(inner: Seq) => inner.toString
    case _ => exprs.mkString("( ", " ", " )")
  }
}

case class Grammar(rules: Map[Id, Expr]) {
  override def toString = {
    val lines = rules.map {
      case (id, expr) => id + " ::= " + expr
    }
    lines.mkString("", "\n", "\n")
  }
}

object EBNF {
  import Parser._
  import Recognizer._
  import Mixfix._
  
  def main(args: Array[String]) {
    val source = "x ::= y + ; y ::= ( 'z' | 'k' ) ; x ::= x x ;"
    val in = source.split("\\s+").toList
    val g = top(in)
    println(in)
    println(g)
  }

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

  val grammar = productions map build
  val top = grammar $

  type Env = Map[Id, Parser[String, Any]]

  def build(productions: List[(Id, Expr)]): Grammar = {
    val grouped = productions.groupBy(_._1)
    val rules = grouped.map {
      case (id, alts) =>
        (id, alts.map(_._2).reduceRight(Alt))
    }
    Grammar(rules)
  }

  def shift(text: String) = {
    lit(text, text)
  }

  def reduce(id: Id, env: () => Env) = parse[String, Any] {
    in0 =>
      val (a, in1) = env()(id)(in0)
      ((id, a), in1)
  }

  def compile(expr: Expr, env: () => Env): Parser[String, Any] = {
    def compile(expr: Expr): Parser[String, Any] = expr match {
      case Tok(text) => shift(text)
      case id: Id => reduce(id, env)
      case Opt(expr) => compile(expr) ?
      case Rep(expr, false) => compile(expr) *
      case Rep(expr, true) => compile(expr) +
      case Alt(expr1, expr2) => compile(expr1) | compile(expr2)
      case Seq(exprs) => parse { Parser.seq(exprs map compile, _) }
    }
    compile(expr)
  }

  def process(grammar: Grammar): Env = {
    // gross hack to enable recursion
    object __ {
      def process(grammar: Grammar, env: () => Env): Env = {
        grammar.rules map {
          case (name, expr) =>
            (name, compile(expr, env))
        }
      }

      val env: Env = process(grammar, () => env)
    }
    __.env
  }
}