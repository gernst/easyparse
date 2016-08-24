// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import scala.util.matching.Regex

trait Parser[I, +A] extends (I => (A, I)) {
  import Parser._
  import Recognizer._

  def |[B >: A](that: Parser[I, B]): Parser[I, B] = parse {
    in =>
      (this: Parser[I, B])(in) or that(in)
  }

  def ~[B](that: Parser[I, B]): Parser[I, (A, B)] = parse {
    in0 =>
      val (a, in1) = this(in0)
      val (b, in2) = that(in1)
      ((a, b), in2)
  }

  def ~(that: Recognizer[I]): Parser[I, A] = parse {
    in0 =>
      val (a, in1) = this(in0)
      val in2 = that(in1)
      (a, in2)
  }

  def >>[B](that: A => Parser[I, B]): Parser[I, B] = parse {
    in =>
      val (a, out) = this(in)
      that(a)(out)
  }
  
  def >>[B](that: A => Recognizer[I]): Recognizer[I] = accept {
    in =>
      val (a, out) = this(in)
      that(a)(out)
  }

  def map[B](f: A => B): Parser[I, B] = parse {
    in =>
      val (a, out) = this(in)
      (f(a), out)
  }

  def ? = (this map Some.apply) | ret(None)
  def * = parse { Parser.rep(this, _: I) }
  def + = lift[I, A, List[A], List[A]](this, _ :: _, this.*)
  def !(msg: String) = this | parse(abort(msg, _))
  def rep(sep: Recognizer[I]) = parse[I, List[A]] { in => repsep(this, sep, in) }

  def foreach(f: A => Unit) = this map { a => f(a); a }
  def filter(p: A => Boolean) = this map { a => if (p(a)) a else fail }
  def filterNot(p: A => Boolean) = this map { a => if (!p(a)) a else fail }
  def collect[B](p: PartialFunction[A, B]) = this map { a => if (p.isDefinedAt(a)) p(a) else fail }
}

object Parser {
  def parse[I, A](f: I => (A, I)) = new Parser[I, A]() {
    def apply(in: I) = f(in)
  }

  def lit[T, A](t: T, a: A): Parser[List[T], A] = parse {
    case `t` :: in => (a, in)
    case _ => fail
  }

  def ret[I, A](a: A): Parser[I, A] = parse {
    case in => (a, in)
  }

  def rec[I, A](p: => Parser[I, A]): Parser[I, A] = parse {
    in => p(in)
  }

  def rep[I, A](p: Parser[I, A], in0: I): (List[A], I) = {
    val (a, in1) = p(in0)
    val (as, in2) = rep(p, in1)
    (a :: as, in2)
  } or {
    (Nil, in0)
  }

  def seq[I, A](ps: List[Parser[I, A]], in0: I): (List[A], I) = ps match {
    case Nil =>
      (Nil, in0)
    case p :: ps =>
      val (a, in1) = p(in0)
      val (as, in2) = seq(ps, in1)
      (a :: as, in2)
  }

  def alt[I, A](ps: List[Parser[I, A]], in: I): (A, I) = ps match {
    case Nil =>
      fail
    case p :: ps =>
      p(in) or alt(ps, in)
  }

  def repsep[I, A](p: Parser[I, A], s: Recognizer[I], in0: I): (List[A], I) = {
    val (a, in1) = p(in0)

    {
      val in2 = s(in1)
      val (as, in3) = repsep(p, s, in2)
      (a :: as, in3)
    } or {
      (List(a), in1)
    }
  }

  def lift[I, A, B, C](p: Parser[I, A], f: (A, B) => C, q: Parser[I, B]): Parser[I, C] = {
    (p ~ q) map f.tupled
  }
  
  def scan(re: Regex) = parse[String, String] {
    in =>
      re.findPrefixOf(in) match {
        case None => fail
        case Some(matched) =>
          val (tok, rest) = in.splitAt(matched.length)
          (tok, rest)
      }
  }

  def __[I] = parse[List[I], I] {
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
  
  implicit class ListParser[A,T](p: Parser[List[T], A]) {
    def $() = {
      in0: List[T] =>
        val (a, in1) = p(in0)
        if(!in1.isEmpty) fail
        a
    }
  }

  implicit class ParseFunction1[A1, B](f: A1 => B) {
    def from[I](p1: Parser[I, A1]) = parse[I, B] {
      in0 =>
        val (a1, in1) = p1(in0)
        (f(a1), in1)
    }
  }

  implicit class ParseFunction2[A1, A2, B](f: (A1, A2) => B) {
    def from[I](p1: Parser[I, A1], p2: Parser[I, A2]) = parse[I, B] {
      in0 =>
        val (a1, in1) = p1(in0)
        val (a2, in2) = p2(in1)
        (f(a1, a2), in2)
    }
  }
  
  implicit class ParseFunction3[A1, A2, A3, B](f: (A1, A2, A3) => B) {
    def from[I](p1: Parser[I, A1], p2: Parser[I, A2], p3: Parser[I, A3]) = parse[I, B] {
      in0 =>
        val (a1, in1) = p1(in0)
        val (a2, in2) = p2(in1)
        val (a3, in3) = p3(in2)
        (f(a1, a2, a3), in3)
    }
  }
  
  implicit class ParseFunction4[A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => B) {
    def from[I](p1: Parser[I, A1], p2: Parser[I, A2], p3: Parser[I, A3], p4: Parser[I, A4]) = parse[I, B] {
      in0 =>
        val (a1, in1) = p1(in0)
        val (a2, in2) = p2(in1)
        val (a3, in3) = p3(in2)
        val (a4, in4) = p4(in3)
        (f(a1, a2, a3, a4), in4)
    }
  }
}
