package arse.ebnf

import scala.language.postfixOps
import arse._

object EBNF {
  import Parser._
  import Recognizer._
  import Mixfix._
  
  def main(args: Array[String]) {
    /*
    val source = "x ::= y + ; y ::= ( 'z' | 'k' ) ; x ::= x x ;"
    val in = source.split("\\s+").toList
    val (g, _) = top(in)
    println(in)
    println(g)
    */
  }
  
  val top = Grammar.grammar

  type Env = Map[Id, Parser[String, Any]]

  def shift(text: String) = {
    ??? // lit(text, text)
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