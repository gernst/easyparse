package arse.examples

import scala.language.postfixOps
import arse.ll._

object JSON {
  import Parser._
  import Recognizer._

  def isId(s: String) = {
    s.head == '"' && s.last == '"'
  }

  val json: Parser[List[String], Any] = Parser.rec(top)

  val _true = lit("true", true)
  val _false = lit("false", false)
  val _null = lit("null", null)

  val id = string filter isId
  val const = _true | _false | _null | int | id

  val array = "[" ~ json.rep(sep = ",") ~ expect("]")

  val pair = id ~ expect(":") ~ json
  val map = "{" ~ pair.rep(sep = ",") ~ expect("}") map (_.toMap)

  val top = array | map | const

  def parse(in: List[String]) = {
    val p = json ! "not a JSON value"
    val (r, _) = p(in)
    r
  }
}
