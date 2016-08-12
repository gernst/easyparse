// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

trait Parser[T, +A] extends (List[T] => (A, List[T])) {
  import Parser._
  import Recognizer._

  def |[B >: A](that: Parser[T, B]): Parser[T, B] = parse {
    in =>
      (this: Parser[T, B])(in) or that(in)
  }

  def ~[B](that: Parser[T, B]): Parser[T, (A, B)] = parse {
    in0 =>
      val (a, in1) = this(in0)
      val (b, in2) = that(in1)
      ((a, b), in2)
  }

  def ~(that: Recognizer[T]): Parser[T, A] = parse {
    in0 =>
      val (a, in1) = this(in0)
      val in2 = that(in1)
      (a, in2)
  }

  def >>[B](that: A => Parser[T, B]): Parser[T, B] = parse {
    in =>
      val (a, out) = this(in)
      that(a)(out)
  }

  def map[B](f: A => B): Parser[T, B] = parse {
    in =>
      val (a, out) = this(in)
      (f(a), out)
  }

  def ? = (this map Some.apply) | ret(None)
  def * = parse { Parser.rep(this, _: List[T]) }
  def + = lift[T, A, List[A], List[A]](this, _ :: _, this.*)
  def !(msg: String) = this | parse(abort(msg, _))
  def rep(sep: Recognizer[T]) = parse[T, List[A]] { in => repsep(this, sep, in) }

  def $(): (List[T] => A) = {
    in =>
      val (a, out) = this(in)
      if (!out.isEmpty) abort("expected end if input", out)
      else a
  }

  def foreach(f: A => Unit) = this map { a => f(a); a }
  def filter(p: A => Boolean) = this map { a => if (p(a)) a else fail }
  def filterNot(p: A => Boolean) = this map { a => if (!p(a)) a else fail }
  def collect[B](p: PartialFunction[A, B]) = this map { a => if (p.isDefinedAt(a)) p(a) else fail }
}

object Parser {
  def parse[T, A](f: List[T] => (A, List[T])) = new Parser[T, A]() {
    def apply(in: List[T]) = f(in)
  }

  def lit[T, A](t: T, a: A): Parser[T, A] = parse {
    case `t` :: in => (a, in)
    case _ => fail
  }

  def ret[T, A](a: A): Parser[T, A] = parse {
    case in => (a, in)
  }

  def rec[T, A](p: => Parser[T, A]): Parser[T, A] = parse {
    in => p(in)
  }

  def rep[T, A](p: Parser[T, A], in0: List[T]): (List[A], List[T]) = {
    val (a, in1) = p(in0)
    val (as, in2) = rep(p, in1)
    (a :: as, in2)
  } or {
    (Nil, in0)
  }

  def seq[T, A](ps: List[Parser[T, A]], in0: List[T]): (List[A], List[T]) = ps match {
    case Nil =>
      (Nil, in0)
    case p :: ps =>
      val (a, in1) = p(in0)
      val (as, in2) = seq(ps, in1)
      (a :: as, in2)
  }

  def alt[T, A](ps: List[Parser[T, A]], in: List[T]): (A, List[T]) = ps match {
    case Nil =>
      fail
    case p :: ps =>
      p(in) or alt(ps, in)
  }

  def repsep[T, A](p: Parser[T, A], s: Recognizer[T], in0: List[T]): (List[A], List[T]) = {
    val (a, in1) = p(in0)

    {
      val in2 = s(in1)
      val (as, in3) = repsep(p, s, in2)
      (a :: as, in3)
    } or {
      (List(a), in1)
    }
  }

  def lift[T, A, B, C](p: Parser[T, A], f: (A, B) => C, q: Parser[T, B]): Parser[T, C] = {
    (p ~ q) map f.tupled
  }

  def __[T] = parse[T, T] {
    case (a :: in) => (a, in)
    case _ => fail
  }

  val string = __[String]
  val boolean = string map { _.toBoolean.mask[IllegalArgumentException] }
  val int = string map { _.toInt.mask[NumberFormatException] }
  val long = string map { _.toLong.mask[NumberFormatException] }
  val float = string map { _.toFloat.mask[NumberFormatException] }
  val double = string map { _.toDouble.mask[NumberFormatException] }
  val bigint = string map { BigInt(_).mask[NumberFormatException] }

  implicit class ParseFunction1[A1, B](f: A1 => B) {
    def from[T](p1: Parser[T, A1]) = parse[T, B] {
      in0 =>
        val (a1, in1) = p1(in0)
        (f(a1), in1)
    }
  }

  implicit class ParseFunction2[A1, A2, B](f: (A1, A2) => B) {
    def from[T](p1: Parser[T, A1], p2: Parser[T, A2]) = parse[T, B] {
      in0 =>
        val (a1, in1) = p1(in0)
        val (a2, in2) = p2(in1)
        (f(a1, a2), in2)
    }
  }
  implicit class ParseFunction3[A1, A2, A3, B](f: (A1, A2, A3) => B) {
    def from[T](p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3]) = parse[T, B] {
      in0 =>
        val (a1, in1) = p1(in0)
        val (a2, in2) = p2(in1)
        val (a3, in3) = p3(in2)
        (f(a1, a2, a3), in3)
    }
  }
  implicit class ParseFunction4[A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => B) {
    def from[T](p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3], p4: Parser[T, A4]) = parse[T, B] {
      in0 =>
        val (a1, in1) = p1(in0)
        val (a2, in2) = p2(in1)
        val (a3, in3) = p3(in2)
        val (a4, in4) = p4(in3)
        (f(a1, a2, a3, a4), in4)
    }
  }
}
