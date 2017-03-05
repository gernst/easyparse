package arse

object ~ {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = {
    Some(p)
  }
}

object parser {
  import control._

  case class Rec[S, A](name: String, p: () => Parser[S, A]) extends Parser[S, A] {
    def apply(s: S) = {
      p()(s)
    }
    def format = {
      name
    }
  }

  case class Ret[S, A](a: A) extends Parser[S, A] {
    def apply(s: S) = {
      (a, s)
    }
    def format = {
      a.toString
    }
  }

  case class Commit[S, A](p: Parser[S, A]) extends Parser[S, A] {
    def apply(s: S) = {
      p(s) or abort(p.toString + " failed", s)
    }
    def format = {
      p.format
    }
  }

  case class Seq[S, A, B](p: Parser[S, A], q: Parser[S, B]) extends Parser[S, (A, B)] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      val (b, s2) = q(s1)
      ((a, b), s2)
    }
    def format = {
      p.format + " ~ " + q.format
    }
  }

  case class Or[S, A](p: Parser[S, A], q: Parser[S, A]) extends Parser[S, A] {
    def apply(s: S) = {
      p(s) or q(s)
    }
    def format = {
      parens(p.format + " | " + q.format)
    }
  }

  case class Rep[S, A](p: Parser[S, A]) extends Parser[S, List[A]] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      val (as, s2) = this(s1)
      (a :: as, s2)
    } or {
      (Nil, s0)
    }
    def format = p match {
      case _: Seq[_, _, _] => parens(p.format) + "*"
      case _: SeqR[_, _] => parens(p.format) + "*"
      case _: SeqP[_, _] => parens(p.format) + "*"
      case _ => p.format + "*"
    }
  }

  case class Map[S, A, B](p: Parser[S, A], f: A => B) extends Parser[S, B] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      (f(a), s1)
    }
    def format = {
      p.format
    }
  }

  case class Filter[S, A](p: Parser[S, A], f: A => Boolean) extends Parser[S, A] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      if (!f(a)) fail
      (a, s1)
    }
    def format = {
      p.format
    }
  }

  case class SeqP[S, A](p: Recognizer[S], q: Parser[S, A]) extends Parser[S, A] {
    def apply(s0: S) = {
      val s1 = p(s0)
      val (a, s2) = q(s1)
      (a, s2)
    }
    def format = {
      p.format + " ~ " + q.format
    }
  }

  case class SeqR[S, A](p: Parser[S, A], q: Recognizer[S]) extends Parser[S, A] {
    def apply(s0: S) = {
      val (a, s1) = p(s0)
      val s2 = q(s1)
      (a, s2)
    }
    def format = {
      p.format + " ~ " + q.format
    }
  }
}