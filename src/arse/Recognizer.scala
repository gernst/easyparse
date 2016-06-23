// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import scala.language.implicitConversions

trait Recognizer[T] extends (List[T] => List[T]) {
  import Parser._
  import Recognizer._

  def |(that: Recognizer[T]): Recognizer[T] = accept {
    in =>
      this(in) or that(in)
  }

  def ~(that: Recognizer[T]): Recognizer[T] = accept {
    in0 =>
      val in1 = this(in0)
      val in2 = that(in1)
      in2
  }

  def ~[B](that: Parser[T, B]): Parser[T, B] = parse {
    in0 =>
      val in1 = this(in0)
      val (b, in2) = that(in1)
      (b, in2)
  }

  def ? = this | skip
  def * = accept { Recognizer.rep(this, _: List[T]) }
  def + = this ~ this.*
  def !(msg: String) = this | accept(abort(msg, _))

  def $(): (List[T] => Unit) = {
    in =>
      val out = this(in)
      if (!out.isEmpty) abort("expected end if input", out)
  }
}

object Recognizer {
  def accept[T](f: List[T] => (List[T])) = new Recognizer[T]() {
    def apply(in: List[T]) = f(in)
  }

  implicit def tok[T](t: T): Recognizer[T] = accept {
    case `t` :: in => in
    case _         => fail
  }

  def skip[T]: Recognizer[T] = accept {
    in => in
  }

  def rec[T](p: => Recognizer[T]): Recognizer[T] = accept {
    in => p(in)
  }

  def rep[T](p: Recognizer[T], in0: List[T]): List[T] = {
    val in1 = p(in0)
    val in2 = rep(p, in1)
    in2
  } or {
    in0
  }

}