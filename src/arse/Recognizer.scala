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

  case class Commit[S](p: Recognizer[S]) extends Recognizer[S] {
    def apply(s: S) = {
      p(s) or abort(p.toString + " failed", s)
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