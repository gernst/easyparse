// ARSE Parser libary
// (c) 2015 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import control._
import scala.annotation.tailrec

trait Parser[T, +A] extends (List[T] => (A, List[T])) {
  def |[B >: A](that: Parser[T, B]): Parser[T, B]
  def ~[B](that: Parser[T, B]): Parser[T, (A, B)]
  def ~>[B](that: Parser[T, B]): Parser[T, B]
  def <~[B](that: Parser[T, B]): Parser[T, A]
  def >>[B](that: A => Parser[T, B]): Parser[T, B]
  def map[B](f: A => B): Parser[T, B]
  def ?(): Parser[T, Option[A]]
  def *(): Parser[T, List[A]]
  def +(): Parser[T, List[A]]
  def !(msg: String): Parser[T, A]
  def $(): (List[T] => A)
  def foreach(f: A => Unit): Parser[T, A]
  def collect[B](p: PartialFunction[A, B]): Parser[T, B]
  def filter(p: A => Boolean): Parser[T, A]
  def filterNot(p: A => Boolean): Parser[T, A]
}

trait Combinators {
  type T
  type Input = List[T]

  trait ParserImpl[+A] extends Parser[T, A] {
    def |[B >: A](that: Parser[T, B]) = lift {
      in =>
        (this: Parser[T, B])(in) or that(in)
    }

    def ~[B](that: Parser[T, B]) = lift {
      in0 =>
        val (a, in1) = this(in0)
        val (b, in2) = that(in1)
        ((a, b), in2)
    }

    def ~>[B](that: Parser[T, B]) = (this ~ that) map { _._2 }
    def <~[B](that: Parser[T, B]) = (this ~ that) map { _._1 }

    def >>[B](that: A => Parser[T, B]) = lift {
      in =>
        val (a, out) = this(in)
        that(a)(out)
    }

    def map[B](f: A => B) = lift {
      (in: Input) =>
        val (a, out) = this(in)
        (f(a), out)
    }

    def ? = (this map Some.apply) | ret(None)
    def !(msg: String) = this | lift { in => error(msg + in.take(12).mkString(" at '", " ", "...'")) }
    def * = lift { sequence(this, _: Input) }
    def + = (this ~ this.*) map { case (a, as) => a :: as }
    // def + = this ~ this.* map { case (a, as) => a :: as }
    def $ = (in: List[T]) => { val (a, _) = (this <~ strict_eof)(in); a }
    def foreach(f: A => Unit) = this map { a => f(a); a }
    def filter(p: A => Boolean) = this map { a => if (p(a)) a else fail }
    def filterNot(p: A => Boolean) = this map { a => if (!p(a)) a else fail }
    def collect[B](p: PartialFunction[A, B]) = this map { a => if (p.isDefinedAt(a)) p(a) else fail }
  }

  def lift[A](f: Input => (A, Input)): Parser[T, A] = new ParserImpl[A]() {
    def apply(in: Input) = f(in)
  }

  def next[A](f: T => A) = lift {
    (in: Input) => if (in.isEmpty) fail else (f(in.head), in.tail)
  }

  def __ = next { t => t }

  def ret[A](a: A) = lift { (a, _) }

  def lit(n: String) = next { t => if (n == t.toString) t else fail }
  def lit[A](n: String, a: A) = next { t => if (n == t.toString) a else fail }

  def tok(t: T) = next { s => if (t == s) t else fail }
  def tok[A](t: T, a: A) = next { s => if (t == s) a else fail }

  def rec[A](p: => Parser[T, A]) = lift { p(_) }

  def repsep[A, B](p: Parser[T, A], q: Parser[T, B]) =
    {
      p ~ (q ~ p).* map { case (a, abs) => a +: abs.map(_._2) }
    } | {
      ret(Nil)
    }

  def until[A](p: Parser[T, A], in: Input): (A, Input) = {
    if (in.isEmpty) fail
    p(in) or until(p, in.tail)
  }

  val eof = lift {
    case Nil => ((), Nil)
    case _ => fail
  }

  val strict_eof = eof ! "unparsed input"

  private def sequence[A](p: Parser[T, A], in0: Input): (List[A], Input) =
    {
      val (a, in1) = p(in0)
      val (as, in2) = sequence(p, in1)
      (a :: as, in2)
    } or {
      (Nil, in0)
    }
}

object Combinators {
  def parse[T, A1, R](f: (A1) => R)(implicit p1: Parser[T, A1]) = (p1) map { case a1 => f(a1) }
  def parse[T, A1, A2, R](f: (A1, A2) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2]) = (p1 ~ p2) map { case (a1, a2) => f(a1, a2) }
  def parse[T, A1, A2, A3, R](f: (A1, A2, A3) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3]) = (p1 ~ p2 ~ p3) map { case ((a1, a2), a3) => f(a1, a2, a3) }
  def parse[T, A1, A2, A3, A4, R](f: (A1, A2, A3, A4) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3], p4: Parser[T, A4]) = (p1 ~ p2 ~ p3 ~ p4) map { case (((a1, a2), a3), a4) => f(a1, a2, a3, a4) }
  def parse[T, A1, A2, A3, A4, A5, R](f: (A1, A2, A3, A4, A5) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3], p4: Parser[T, A4], p5: Parser[T, A5]) = (p1 ~ p2 ~ p3 ~ p4 ~ p5) map { case ((((a1, a2), a3), a4), a5) => f(a1, a2, a3, a4, a5) }
  def parse[T, A1, A2, A3, A4, A5, A6, R](f: (A1, A2, A3, A4, A5, A6) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3], p4: Parser[T, A4], p5: Parser[T, A5], p6: Parser[T, A6]) = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6) map { case (((((a1, a2), a3), a4), a5), a6) => f(a1, a2, a3, a4, a5, a6) }
}
