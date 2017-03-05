package arse

object ~ {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = {
    Some(p)
  }
}

object parser {
  import control._

  case class Rec[T, A](name: String, p: () => Parser[T, A]) extends Parser[T, A] {
    def apply(t: T) = {
      p()(t)
    }
    def format = {
      name
    }
  }

  case class Ret[T, A](a: A) extends Parser[T, A] {
    def apply(t: T) = {
      (a, t)
    }
    def format = {
      a.toString
    }
  }

  case class Commit[T, A](p: Parser[T, A]) extends Parser[T, A] {
    def apply(t: T) = {
      p(t) or abort(p.toString + " failed", t)
    }
    def format = {
      p.format
    }
  }

  case class Seq[T, A, B](p: Parser[T, A], q: Parser[T, B]) extends Parser[T, (A, B)] {
    def apply(t0: T) = {
      val (a, t1) = p(t0)
      val (b, t2) = q(t1)
      ((a, b), t2)
    }
    def format = {
      p.format + " ~ " + q.format
    }
  }

  case class Or[T, A](p: Parser[T, A], q: Parser[T, A]) extends Parser[T, A] {
    def apply(t: T) = {
      p(t) or q(t)
    }
    def format = {
      parens(p.format + " | " + q.format)
    }
  }

  case class Rep[T, A](p: Parser[T, A]) extends Parser[T, List[A]] {
    def apply(t0: T) = {
      val (a, t1) = p(t0)
      val (as, t2) = this(t1)
      (a :: as, t2)
    } or {
      (Nil, t0)
    }
    def format = p match {
      case _: Seq[_, _, _] => parens(p.format) + "*"
      case _: SeqR[_, _] => parens(p.format) + "*"
      case _: SeqP[_, _] => parens(p.format) + "*"
      case _ => p.format + "*"
    }
  }

  case class Map[T, A, B](p: Parser[T, A], f: A => B) extends Parser[T, B] {
    def apply(t0: T) = {
      val (a, t1) = p(t0)
      (f(a), t1)
    }
    def format = {
      p.format
    }
  }

  case class Filter[T, A](p: Parser[T, A], f: A => Boolean) extends Parser[T, A] {
    def apply(t0: T) = {
      val (a, t1) = p(t0)
      if (!f(a)) fail
      (a, t1)
    }
    def format = {
      p.format
    }
  }

  case class SeqP[T, A](p: Recognizer[T], q: Parser[T, A]) extends Parser[T, A] {
    def apply(t0: T) = {
      val t1 = p(t0)
      val (a, t2) = q(t1)
      (a, t2)
    }
    def format = {
      p.format + " ~ " + q.format
    }
  }

  case class SeqR[T, A](p: Parser[T, A], q: Recognizer[T]) extends Parser[T, A] {
    def apply(t0: T) = {
      val (a, t1) = p(t0)
      val t2 = q(t1)
      (a, t2)
    }
    def format = {
      p.format + " ~ " + q.format
    }
  }
}