// ARSE Parser libary
// (c) 2020 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

import scala.language.implicitConversions

package object arse {
  type ~[+A, +B] = Tuple2[A, B]
  val ~ = Tuple2

  implicit def input(text: String)(implicit w: Whitespace) = {
    new Input(text, 0, w)
  }

  implicit class toLit(s: String) {
    def ~[A](q: Parser[A]) = new Literal(s) ~> q
    def ?~[A](q: Parser[A]) = new Literal(s) ?~> q
  }

  case class Error(msg: String, in: Input) extends Exception {
    override def toString = msg + " at '" + (in.rest take 32) + "...'"
  }

  def fail(msg: String, in: Input, cm: Boolean, cause: Throwable = null) = {
    if (cm) {
      throw Error(msg, in) initCause cause
    } else {
      backtrack()
    }
  }

  def ret[A](a: A) = new Accept(a)
  
  def L(tokens: String*) = new Literals(tokens: _*)

  def int = S("[+-]?[0-9]+") map {
    str => str.toInt
  }
  
  def bigint = S("[+-]?[0-9]+") map {
    str => BigInt(str)
  }

  def double = S("[+-]?[0-9]+[.]?[0-9]*") map {
    str => str.toDouble
  }

  def char = S("\'([^\']|\\')\'") map {
    str =>
      str.substring(1, str.length - 1)
  }

  def string = S("\"[^\"]*\"") map {
    str =>
      str.substring(1, str.length - 1)
  }

  def S(pattern: String)(implicit name: sourcecode.Name): Parser[String] = {
    new Regex(name.value, pattern)
  }

  def M[Op, Expr](
    p: => Parser[Expr],
    op: Parser[Op],
    ap: (Op, List[Expr]) => Expr,
    s: Syntax[Op],
    min: Int = Int.MinValue,
    max: Int = Int.MaxValue)(implicit name: sourcecode.Name) = {
    Mixfix[Op, Expr](name.value, () => p, ap, s prefix_op op, s postfix_op op, s infix_op op, min, max)
  }
  
  def P[A](p: => Parser[A])(implicit name: sourcecode.Name): Parser[A] = {
    new Recursive(name.value, () => p)
  }
  
  trait NoStackTrace {
    this: Throwable =>
    override def fillInStackTrace = this
    override val getStackTrace = Array[StackTraceElement]()
  }

  def backtrack() = {
    throw Backtrack()
  }

  case class Backtrack() extends Throwable /* with NoStackTrace */ {
    override def toString = "backtrack"
  }

  implicit class Control[A](first: => A) {
    def or[B <: A](second: => B) = {
      try {
        first
      } catch {
        case Backtrack() =>
          second
      }
    }
  }
}
