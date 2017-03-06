// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

object recognizer {
  import control._

  case class Rec[S](name: String, p: () => Recognizer[S]) extends Recognizer[S] {
    def apply(s: S) = {
      p()(s)
    }
    def format = {
      name
    }
  }

  case class Accept[S]() extends Recognizer[S] {
    def apply(s: S) = {
      s
    }
    def format = {
      "()"
    }
  }

  case class Lit[T](t: T) extends Recognizer[List[T]] {
    def apply(s: List[T]) = s match {
      case `t` :: s => s
      case _ => fail
    }
    def format = t match {
      case c: Char => "'" + c + "'"
      case s: String => "\"" + s + "\""
      case _ => t.toString
    }
  }

  case class Drop[S, A](p: Parser[S, A]) extends Recognizer[S] {
    def apply(s0: S) = {
      val (_, s1) = p(s0)
      s1
    }
    def format = p match {
      case _: arse.Seq => "?" + parens(p.format)
      case _ => "?" + p.format
    }
  }

  case class Commit[S](p: Recognizer[S]) extends Recognizer[S] {
    def apply(s: S) = {
      p(s) or abort("expected " + p.format, s)
    }
    def format = {
      p.format
    }
  }

  case class Seq[S](p: Recognizer[S], q: Recognizer[S]) extends Recognizer[S] with arse.Seq {
    def apply(s0: S) = {
      val s1 = p(s0)
      val s2 = q(s1)
      s2
    }
    def format = {
      p.format + " ~ " + q.format
    }
  }

  case class Or[S](p: Recognizer[S], q: Recognizer[S]) extends Recognizer[S] {
    def apply(s: S) = {
      p(s) or q(s)
    }
    def format = {
      parens(p.format + " | " + q.format)
    }
  }

  case class Rep[S](p: Recognizer[S]) extends Recognizer[S] {
    def apply(s0: S) = {
      val s1 = p(s0)
      val s2 = this(s1)
      s2
    } or {
      s0
    }
    def format = p match {
      case _: arse.Seq => parens(p.format) + "*"
      case _ => p.format + "*"
    }
  }
}