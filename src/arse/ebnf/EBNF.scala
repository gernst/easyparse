package arse.ebnf

import scala.language.postfixOps
import arse._

object EBNF {
  import Parser._
  import Recognizer._
  import Mixfix._

  def main(args: Array[String]) {
    // val in = "start ::= ( 'x' | 'y' ) + num = '\\d+' end ; end ::= 'z' ;"
    val in = "start ::= 'x' start | 'y' ;"
    val source = in.split("\\s+").toList
    println(source)
    val (g, _) = top(source)
    println("grammar\n" + g)
    val env = process(g)
    println(env)
    val p = env(Id("start"))
    val test = "xxxy"
    val (x, out) = p(test)
    println("result: " + x)
    println("remaining: " + out)
  }

  val top = Grammar.grammar

  type Env = Map[Id, Parser[String, Any]]

  def shift(text: String) = {
    val pat = text.substring(1, text.length - 1)
    scan(pat.r)
  }

  def reduce(id: Id, env: () => Env) = parse[String, Any] {
    in0 =>
      val (a, in1) = env()(id)(in0)
      (a, in1)
  }

  def compile(expr: Expr, env: () => Env): Parser[String, Any] = {
    def compile(expr: Expr): Parser[String, Any] = expr match {
      case Tok(text) => shift(text)
      case id: Id => reduce(id, env)
      case Opt(expr) => compile(expr) ?
      case Named(Id(name), expr) => compile(expr) map { (name, _) }
      case Rep(expr, false) => compile(expr) *
      case Rep(expr, true) => compile(expr) +
      case Alt(expr1, expr2) => compile(expr1) | compile(expr2)
      case Seq(List(expr)) => compile(expr)
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
      println(grammar)
      println(env)
    }
    __.env
  }
}