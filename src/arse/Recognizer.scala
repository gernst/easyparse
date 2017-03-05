package arse

object recognizer {
  import control._

  case class Rec[T](name: String, p: () => Recognizer[T]) extends Recognizer[T] {
    def apply(t: T) = {
      p()(t)
    }
    def format = {
      name
    }
  }

  case class Accept[T]() extends Recognizer[T] {
    def apply(t: T) = {
      t
    }
    def format = {
      "()"
    }
  }

  case class Commit[T](p: Recognizer[T]) extends Recognizer[T] {
    def apply(t: T) = {
      p(t) or abort(p.toString + " failed", t)
    }
    def format = {
      p.format
    }
  }

  case class Seq[T](p: Recognizer[T], q: Recognizer[T]) extends Recognizer[T] {
    def apply(t0: T) = {
      val t1 = p(t0)
      val t2 = q(t1)
      t2
    }
    def format = {
      p.format + " ~ " + q.format
    }
  }

  case class Or[T](p: Recognizer[T], q: Recognizer[T]) extends Recognizer[T] {
    def apply(t: T) = {
      p(t) or q(t)
    }
    def format = {
      parens(p.format + " | " + q.format)
    }
  }

  case class Rep[T](p: Recognizer[T]) extends Recognizer[T] {
    def apply(t0: T) = {
      val t1 = p(t0)
      val t2 = this(t1)
      t2
    } or {
      t0
    }
    def format = p match {
      case _: Seq[_] => parens(p.format) + "*"
      case _ => p.format + "*"
    }
  }
}