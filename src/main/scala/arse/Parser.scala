// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import java.util.regex.Pattern

import control._

object parser {
  case class Rec[A](name: String, p: () => Parser[A]) extends Parser[A] {
    def apply(in: Input) = {
      p()(in)
    }
  }

  case class Accept[+A](a: A) extends Parser[A] {
    def apply(in: Input) = {
      a
    }
  }

  case class Regex(pattern: String) extends Parser[String] {
    val regex = Pattern.compile(pattern)
    val matcher = regex.matcher("")

    def apply(in: Input) = {
      matcher.reset(in.text)

      if (matcher.find(in.position)) {
        in.position = matcher.end()
        matcher.group()
      } else {
        fail(in)
      }
    }
  }

  case class Seq[+A, +B](p: Parser[A], q: Parser[B]) extends Parser[A ~ B] {
    def apply(in: Input) = {
      val a = p(in)
      val b = q(in)
      (a, b)
    }
  }

  case class Look[+A](p: Parser[A]) extends Parser[A] {
    def apply(in: Input) = {
      val back = in.position

      {
        p(in)
      } rollback {
        in.position = back
        backtrack()
      }
    }
  }

  case class Or[+A](p: Parser[A], q: Parser[A]) extends Parser[A] {
    def apply(in: Input) = {
      val back = in.position

      {
        in.position = back
        p(in)
      } or {
        in.position = back
        q(in)
      } rollback {
        in.position = back
      }
    }
  }

  case class Rep[+A](p: Parser[A]) extends Parser[List[A]] {
    val q = p.look

    def apply(in: Input) = {
      val a = q(in)
      val as = this(in)
      a :: as
    } or {
      Nil
    }
  }

  case class Map[A, +B](p: Parser[A], f: A => B) extends Parser[B] {
    def apply(in: Input) = {
      val a = p(in)
      try {
        f(a)
      } catch {
        case cause: Exception =>
          fail(in, cause)
      }
    }
  }

  /* case class FlatMap[A, B](p: Parser[A], q: A => Parser[B]) extends Parser[B] {
    def apply(in: Input) = {
      val a = p(in)
      val b = q(a)(in)
      b
    }
  } */

  case class SeqP[+A](p: Recognizer, q: Parser[A]) extends Parser[A] {
    def apply(in: Input) = {
      val a = p(in)
      val b = q(in)
      b
    }
  }

  case class SeqR[+A](p: Parser[A], q: Recognizer) extends Parser[A] {
    def apply(in: Input) = {
      val a = p(in)
      val b = q(in)
      a
    }
  }

  case class Seq1[A1, +B](p1: Parser[A1], f: (A1) => B) extends Parser[B] {
    def apply(in: Input) = {
      val a1 = p1(in)
      f(a1)
    }
  }

  case class Seq2[A1, A2, +B](p1: Parser[A1], p2: Parser[A2], f: (A1, A2) => B) extends Parser[B] {
    def apply(in: Input) = {
      val a1 = p1(in)
      val a2 = p2(in)
      f(a1, a2)
    }
  }

  case class Seq3[A1, A2, A3, +B](p1: Parser[A1], p2: Parser[A2], p3: Parser[A3], f: (A1, A2, A3) => B) extends Parser[B] {
    def apply(in: Input) = {
      val a1 = p1(in)
      val a2 = p2(in)
      val a3 = p3(in)
      f(a1, a2, a3)
    }
  }

  case class Seq4[A1, A2, A3, A4, +B](p1: Parser[A1], p2: Parser[A2], p3: Parser[A3], p4: Parser[A4], f: (A1, A2, A3, A4) => B) extends Parser[B] {
    def apply(in: Input) = {
      val a1 = p1(in)
      val a2 = p2(in)
      val a3 = p3(in)
      val a4 = p4(in)
      f(a1, a2, a3, a4)
    }
  }
}