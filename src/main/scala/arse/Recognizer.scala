// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

object recognizer {
  import control._

  case class Rec(name: String, p: () => Recognizer) extends Recognizer {
    def apply(in: Input) = {
      p()(in)
    }
  }

  case class Lit(token: String) extends Recognizer {
    def apply(in: Input) = {
      if (in.text.startsWith(token, in.position)) {
        in.position += token.length
      } else {
        fail
      }
    }
  }

  case object EOF extends Recognizer {
    def apply(in: Input) = {
      if (!in.isEmpty) fail
    }
  }

  case object Fail extends Recognizer {
    def apply(in: Input) = {
      fail
    }
  }

  case object Accept extends Recognizer {
    def apply(in: Input) = {
    }
  }

  case class Drop[S, +A](p: Parser[A]) extends Recognizer {
    def apply(in: Input) = {
      p(in)
    }
  }

  case class Commit(p: Recognizer) extends Recognizer {
    def apply(in: Input) = {
      p(in) or abort("expected " + p, in)
    }
  }

  case class Seq(p: Recognizer, q: Recognizer) extends Recognizer {
    def apply(in: Input) = {
      p(in)
      q(in)
    }
  }

  case class Or(p: Recognizer, q: Recognizer) extends Recognizer {
    def apply(in: Input) = {
      val backtrack = in.position;
      { p(in) } or { in.position = backtrack; q(in) }
    }
  }

  case class Rep(p: Recognizer) extends Recognizer {
    def apply(in: Input) = {
      p(in)
      this(in)
    } or {
    }
  }
}