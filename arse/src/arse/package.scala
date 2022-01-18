// ARSE Parser libary
// (c) 2022 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

import scala.language.implicitConversions

package object arse {
  type ~[+A, +B] = Tuple2[A, B]
  val ~ = Tuple2

  type Result[+A, T] = (A, Input[T])
  type Input[T] = Seq[T]

  case class Error(msg: String, in: Input[_]) extends Exception {
    override def toString = msg + " at '" + (in take 32) + "...'"
  }

  def fail(msg: String, in: Input[_], cm: Boolean, cause: Throwable = null) = {
    if (cm) {
      throw Error(msg, in) initCause cause
    } else {
      backtrack()
    }
  }

  def ret[A, T](a: A) =
    new Parser.Accept[A, T](a)
  def tok[T] =
    new Parser.Shift[T]()

  def KW(name: String) =
    new Scanner.Keyword(name)
  def KW(implicit name: sourcecode.Name) =
    new Scanner.Keyword(name.value)

  def V(name: String) =
    new Scanner.Value(name)
  def V(implicit name: sourcecode.Name) =
    new Scanner.Value(name.value)

  def P[A, T](
      p: => Parser[A, T]
  )(implicit name: sourcecode.Name): Parser[A, T] = {
    new Parser.Recursive(name.value, () => p)
  }

  def M[Op, Expr, T](
      p: => Parser[Expr, T],
      op: Parser[Op, T],
      ap: (Op, List[Expr]) => Expr,
      s: Syntax[Op, T],
      min: Int = Int.MinValue,
      max: Int = Int.MaxValue
  )(implicit name: sourcecode.Name) = {
    Mixfix[Op, Expr, T](
      name.value,
      () => p,
      ap,
      s prefix_op op,
      s postfix_op op,
      s infix_op op,
      min,
      max
    )
  }

  trait NoStackTrace {
    this: Throwable =>
    override def fillInStackTrace = this
    override val getStackTrace = Array[StackTraceElement]()
  }

  def backtrack() = {
    throw Backtrack()
  }

  case class Backtrack() extends Throwable with NoStackTrace {
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

  // def L[T](tokens: T*) = tok[T] filter tokens.contains

  /* def int = S("[+-]?[0-9]+") map {
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
  } */
}
