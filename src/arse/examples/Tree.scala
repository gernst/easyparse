package arse.examples

import arse._

trait Tree {
  def start: Int
  def end: Int
  def length = end - start
}

case class Leaf(text: String, start: Int) extends Tree {
  val end = start + text.length
}

case class Node(sub: List[Tree]) extends Tree {
  val start = sub.map(_.start).min
  val end = sub.map(_.end).max
}

object TreeParsers extends Combinators {
  type T = Token

  implicit def leaf(txt: String) = lit(txt) map {
    tok => Leaf(tok.text, tok.start)
  }

  def node(ps: List[Parser[Token, Tree]]): Parser[Token, List[Tree]] = ps match {
    case Nil =>
      ret(Nil)
    case p :: rest =>
      p >> { t => node(rest) map (t :: _) }
  }

  object $ {
    def apply(ps: Parser[Token, Tree]*): Parser[Token, Tree] = {
      node(ps.toList) map Node
    }

    def unapplySeq(t: Any): Option[List[Any]] = t match {
      case Leaf(text, _) =>
        Some(List(text))
      case Node(sub) =>
        Some(sub)
    }
  }
}

object Test {
  import TreeParsers._

  def main(args: Array[String]) {
    val in = List(Token("x", 0), Token("y", 2))
    val p = $("x", "y")
    val (r, _) = p(in)
    r match {
      case $($("x"), y) => println(y)
    }
  }
}