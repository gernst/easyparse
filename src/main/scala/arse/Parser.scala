// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

object parser {
  import control._

  case class Rec[S, A](name: String, p: () => Parser[S, A]) extends Parser[S, A] {
    def apply(s: S) = {
      p()(s)
    }
  }

  case class Tok[T]() extends Parser[List[T], T] {
    def apply(s: List[T]) = s match {
      case t :: s => (t, s)
      case _ => fail
    }
  }

  case class Ret[S, +A](a: A) extends Parser[S, A] {
    def apply(s: S) = {
      (a, s)
    }
  }

  case class Commit[S, +A](p: Parser[S, A]) extends Parser[S, A] {
    def apply(s: S) = {
      p(s) or abort("expected " + p, s)
    }
  }

  case class Seq[S, +A, +B](p: Parser[S, A], q: Parser[S, B]) extends Parser[S, A ~ B] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      val (b, s2) = q(s1)
      (new ~(a,  b), s2)
    }
  }

  case class Or[S, +A](p: Parser[S, A], q: Parser[S, A]) extends Parser[S, A] {
    def apply(s: S) = {
      p(s) or q(s)
    }
  }

  case class Rep[S, +A](p: Parser[S, A]) extends Parser[S, List[A]] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      val (as, s2) = this(s1)
      (a :: as, s2)
    } or {
      (Nil, s0)
    }
  }

  case class Map[S, A, +B](p: Parser[S, A], f: A => B) extends Parser[S, B] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      (f(a), s1)
    }
  }

  case class Filter[S, A](p: Parser[S, A], f: A => Boolean) extends Parser[S, A] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      if (!f(a)) fail
      (a, s1)
    }
  }

  case class FlatMap[S, A, B](p: Parser[S, A], q: A => Parser[S, B]) extends Parser[S, B] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      val (b, s2) = q(a)(s1)
      (b, s2)
    }
  }

  case class SeqP[S, +A](p: Recognizer[S], q: Parser[S, A]) extends Parser[S, A] {
    def apply(s0: S) = {
      val s1 = p(s0)
      val (a, s2) = q(s1)
      (a, s2)
    }
  }

  case class SeqR[S, +A](p: Parser[S, A], q: Recognizer[S]) extends Parser[S, A] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      val s2 = q(s1)
      (a, s2)
    }
  }

  case class Seq1[S, A1, +B](p1: Parser[S, A1], f: (A1) => B) extends Parser[S, B] {
    def apply(s0: S) = {
      val (a1, s1) = p1(s0)
      (f(a1), s1)
    }
  }

  case class Seq2[S, A1, A2, +B](p1: Parser[S, A1], p2: Parser[S, A2], f: (A1, A2) => B) extends Parser[S, B] {
    def apply(s0: S) = {
      val (a1, s1) = p1(s0)
      val (a2, s2) = p2(s1)
      (f(a1, a2), s2)
    }
  }

  case class Seq3[S, A1, A2, A3, +B](p1: Parser[S, A1], p2: Parser[S, A2], p3: Parser[S, A3], f: (A1, A2, A3) => B) extends Parser[S, B] {
    def apply(s0: S) = {
      val (a1, s1) = p1(s0)
      val (a2, s2) = p2(s1)
      val (a3, s3) = p3(s2)
      (f(a1, a2, a3), s3)
    }
  }

  case class Seq4[S, A1, A2, A3, A4, +B](p1: Parser[S, A1], p2: Parser[S, A2], p3: Parser[S, A3], p4: Parser[S, A4], f: (A1, A2, A3, A4) => B) extends Parser[S, B] {
    def apply(s0: S) = {
      val (a1, s1) = p1(s0)
      val (a2, s2) = p2(s1)
      val (a3, s3) = p3(s2)
      val (a4, s4) = p4(s3)
      (f(a1, a2, a3, a4), s4)
    }
  }
}