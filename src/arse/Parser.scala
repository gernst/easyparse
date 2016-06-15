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

  /*  def !(msg: String) = this | parse { in => error(msg + in.take(12).mkString(" at '", " ", "...'")) }
  def $ = this ~ eof */

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
    case _         => fail
  }

  def ret[T, A](a: A): Parser[T, A] = parse {
    case in => (a, in)
  }

  def rec[T, A](p: Parser[T, A]): Parser[T, A] = parse {
    in => p(in)
  }

  def rep[T, A](p: Parser[T, A], in0: List[T]): (List[A], List[T]) = {
    val (a, in1) = p(in0)
    val (as, in2) = rep(p, in1)
    (a :: as, in2)
  } or {
    (Nil, in0)
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
    case _         => fail
  }

  val string = __[String]
  val boolean = string map { _.toBoolean.mask[IllegalArgumentException] }
  val int = string map { _.toInt.mask[NumberFormatException] }
  val long = string map { _.toLong.mask[NumberFormatException] }
  val float = string map { _.toFloat.mask[NumberFormatException] }
  val double = string map { _.toDouble.mask[NumberFormatException] }
  val bigint = string map { BigInt(_).mask[NumberFormatException] }
}
