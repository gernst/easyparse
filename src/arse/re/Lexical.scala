package arse.re

case class Lexical(gs: Map[String, Regex]) extends RegexLike {
  def first = gs.foldLeft(Letters.empty) {
    case (cs, (n, e)) => cs | e.first
  }

  def isNullable = gs exists {
    case (n, e) => e.isNullable
  }

  def derive(c: Letter) = Lexical(gs.map {
    case (n, e) => (n, e derive c)
  })

  override def toString = {
    val ss = gs.map {
      case (n, e) => n + ":" + e
    }
    ss.mkString("{", " | ", "}")
  }
}