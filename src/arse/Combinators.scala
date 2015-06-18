package arse

import control._

trait Parser[T, +A] extends (Seq[T] => (A, Seq[T])) {
  def |[B >: A](that: Parser[T, B]): Parser[T, B]
  def ~[B](that: Parser[T, B]): Parser[T, (A, B)]
  def ~>[B](that: Parser[T, B]): Parser[T, B]
  def <~[B](that: Parser[T, B]): Parser[T, A]
  def >>[B](that: A => Parser[T, B]): Parser[T, B]
  def ^^[B](f: A => B): Parser[T, B]
  def ?(): Parser[T, Option[A]]
  def *(): Parser[T, Seq[A]]
}

trait Combinators {
  type T
  type Input = Seq[T]

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

    def ~>[B](that: Parser[T, B]) = (this ~ that) ^^ { _._2 }
    def <~[B](that: Parser[T, B]) = (this ~ that) ^^ { _._1 }

    def >>[B](that: A => Parser[T, B]) = lift {
      in =>
        val (a, out) = this(in)
        that(a)(out)
    }

    def ^^[B](f: A => B) = lift {
      (in: Input) =>
        val (a, out) = this(in)
        (f(a), out)
    }

    def ? = (this ^^ Some.apply) | ret(None)
    // def !(msg: String) = this | ret(error(msg))
    def * = lift { map(this, _: Input) }
    // def + = this ~ this.* ^^ { case (a, as) => a :: as }
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

  private def map[A](p: Parser[T, A], in0: Input): (List[A], Input) =
    {
      val (a, in1) = p(in0)
      val (as, in2) = map(p, in1)
      (a :: as, in2)
    } or {
      (Nil, in0)
    }

}

object Combinators {
  def parse[T, A1, R](f: (A1) => R)(implicit p1: Parser[T, A1]) = (p1) ^^ { case a1 => f(a1) }
  def parse[T, A1, A2, R](f: (A1, A2) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2]) = (p1 ~ p2) ^^ { case (a1, a2) => f(a1, a2) }
  def parse[T, A1, A2, A3, R](f: (A1, A2, A3) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3]) = (p1 ~ p2 ~ p3) ^^ { case ((a1, a2), a3) => f(a1, a2, a3) }
  def parse[T, A1, A2, A3, A4, R](f: (A1, A2, A3, A4) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3], p4: Parser[T, A4]) = (p1 ~ p2 ~ p3 ~ p4) ^^ { case (((a1, a2), a3), a4) => f(a1, a2, a3, a4) }
  def parse[T, A1, A2, A3, A4, A5, R](f: (A1, A2, A3, A4, A5) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3], p4: Parser[T, A4], p5: Parser[T, A5]) = (p1 ~ p2 ~ p3 ~ p4 ~ p5) ^^ { case ((((a1, a2), a3), a4), a5) => f(a1, a2, a3, a4, a5) }
  def parse[T, A1, A2, A3, A4, A5, A6, R](f: (A1, A2, A3, A4, A5, A6) => R)(implicit p1: Parser[T, A1], p2: Parser[T, A2], p3: Parser[T, A3], p4: Parser[T, A4], p5: Parser[T, A5], p6: Parser[T, A6]) = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6) ^^ { case (((((a1, a2), a3), a4), a5), a6) => f(a1, a2, a3, a4, a5, a6) }
}