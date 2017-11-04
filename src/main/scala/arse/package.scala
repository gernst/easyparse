// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

import scala.language.implicitConversions
import java.util.regex.Pattern

package object arse {
  import control._

  type ~[+A, +B] = Tuple2[A, B]
  val ~ = Tuple2
  val $ = recognizer.EOF

  type Recognizer = recognizer.Recognizer
  type Parser[+A] = parser.Parser[A]

  trait WithPattern {
    def pattern: String

    val regex = Pattern.compile(pattern)
    val matcher = regex.matcher("")

    def matches(text: String, pos: Int) = {
      matcher.reset(text)
      matcher.region(pos, text.length)
      // print("match " + pattern + " at '" + text.substring(pos) + "' ")

      if (matcher.lookingAt()) {
        val end = matcher.end
        // println()
        // println("remaining input '" + text.substring(end) + "'")
        Some(end)
      } else {
        // println("failed")
        None
      }
    }
  }

  trait WithFailure {
    def fail(in: Input, cause: Throwable = null) = {
      if (in.commit) {
        val message = this + " failed"
        println("in fail: " + message + " at '" + in.rest + "'")
        throw Failure(message, in, cause)
      } else {
        throw Backtrack
      }
    }
  }

  case class Whitespace(pattern: String) extends WithPattern {
  }

  object Whitespace {
    val default = Whitespace("\\s*")
  }

  case class Failure(message: String, in: Input, cause: Throwable = null) extends Exception(message, cause) with NoStackTrace

  implicit def input(text: String)(implicit w: Whitespace) = {
    new Input(text, 0, true, w)
  }

  val accept = recognizer.Accept
  def ret[A](a: A) = parser.Accept(a)

  implicit def lit(text: String): Recognizer = recognizer.Lit(text)
  def lit[A](text: String, a: A): Parser[A] = recognizer.Lit(text) map a

  def scan(pattern: String): Parser[String] = parser.Regex(pattern)

  def int = scan("[+-]?[0-9]+") map {
    str => str.toInt
  }

  def double = scan("[+-]?[0-9]+[.]?[0-9]*") map {
    str => str.toDouble
  }

  def char = scan("\'([^\']|\\')\'") map {
    str =>
      str.substring(1, str.length - 1)
  }

  def string = scan("\"[^\"]*\"") map {
    str =>
      str.substring(1, str.length - 1)
  }

  def mixfix[O, E](p: => Parser[E],
    op: String => O,
    ap: (O, List[E]) => E,
    s: Syntax[O],
    min: Int = Int.MinValue,
    max: Int = Int.MaxValue)(implicit name: sourcecode.Name) = {
    ???
    //Mixfix[List[T], O, E](name.value, () => p, ap, s prefix_op op, s postfix_op op, s infix_op op, min, max)
  }

  def P[A](p: => Parser[A])(implicit name: sourcecode.Name): Parser[A] = {
    parser.Rec(name.value, () => p)
  }

  def P(p: => Recognizer)(implicit name: sourcecode.Name): Recognizer = {
    recognizer.Rec(name.value, () => p)
  }
}