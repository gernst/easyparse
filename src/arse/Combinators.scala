package arse

import control._

trait Combinators {
  type Token
  type Input = Seq[Token]

  trait Parser[+A] extends (Input => (A, Input)) {
    def |[B >: A](that: Parser[B]) = lift {
      in =>
        (this: Parser[B])(in) or that(in)
    }

    def ~[B](that: Parser[B]) = lift {
      in0 =>
        val (a, in1) = this(in0)
        val (b, in2) = that(in1)
        ((a, b), in2)
    }
    
    def ~>[B](that: Parser[B]) = (this ~ that) ^^ { _._2 }
    def <~[B](that: Parser[B]) = (this ~ that) ^^ { _._1 }

    def ^^[B](f: A => B) = lift {
      in =>
        val (a, out) = this(in)
        (f(a), out)
    }

    def ? = (this ^^ Some.apply) | ret(None)
    def !(msg: String) = this | ret(error(msg))
    def * = lift { map(this, _) }
    // def + = this ~ this.* ^^ { case (a, as) => a :: as }
  }

  def lift[A](f: Input => (A, Input)) = new Parser[A]() {
    def apply(in: Input) = f(in)
  }

  def next[A](f: (Token) => (A)) = lift {
    case t :: in =>
      // println("at " + t)
      (f(t), in)
    case _ =>
      fail
  }

  def __ = next { t => t }

  def ret[A](a: A) = lift { (a, _) }

  def lit(n: String) = next { t => if (n == t.toString) t else fail }
  def lit[A](n: String, a: A) = next { t => if (n == t.toString) a else fail }

  def tok(t: Token) = next { s => if (t == s) t else fail }
  def tok[A](t: Token, a: A) = next { s => if (t == s) a else fail }

  def rec[A](p: => Parser[A]) = lift { p(_) }

  private def map[A](p: Parser[A], in0: Input): (List[A], Input) =
    {
      val (a, in1) = p(in0)
      val (as, in2) = map(p, in1)
      (a :: as, in2)
    } or {
      (Nil, in0)
    }

  def parse[A1, R](f: (A1) => R)(implicit p1: Parser[A1]) = (p1) ^^ { case a1 => f(a1) }
  def parse[A1, A2, R](f: (A1, A2) => R)(implicit p1: Parser[A1], p2: Parser[A2]) = (p1 ~ p2) ^^ { case (a1, a2) => f(a1, a2) }
  def parse[A1, A2, A3, R](f: (A1, A2, A3) => R)(implicit p1: Parser[A1], p2: Parser[A2], p3: Parser[A3]) = (p1 ~ p2 ~ p3) ^^ { case ((a1, a2), a3) => f(a1, a2, a3) }
  def parse[A1, A2, A3, A4, R](f: (A1, A2, A3, A4) => R)(implicit p1: Parser[A1], p2: Parser[A2], p3: Parser[A3], p4: Parser[A4]) = (p1 ~ p2 ~ p3 ~ p4) ^^ { case (((a1, a2), a3), a4) => f(a1, a2, a3, a4) }
  def parse[A1, A2, A3, A4, A5, R](f: (A1, A2, A3, A4, A5) => R)(implicit p1: Parser[A1], p2: Parser[A2], p3: Parser[A3], p4: Parser[A4], p5: Parser[A5]) = (p1 ~ p2 ~ p3 ~ p4 ~ p5) ^^ { case ((((a1, a2), a3), a4), a5) => f(a1, a2, a3, a4, a5) }
  def parse[A1, A2, A3, A4, A5, A6, R](f: (A1, A2, A3, A4, A5, A6) => R)(implicit p1: Parser[A1], p2: Parser[A2], p3: Parser[A3], p4: Parser[A4], p5: Parser[A5], p6: Parser[A6]) = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6) ^^ { case (((((a1, a2), a3), a4), a5), a6) => f(a1, a2, a3, a4, a5, a6) }
}