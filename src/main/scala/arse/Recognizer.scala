// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import control._

object recognizer {
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
        fail(in)
      }
    }
  }

  case object EOF extends Recognizer {
    def apply(in: Input) = {
      if (!in.isEmpty)
        fail(in)
    }
  }

  case object Fail extends Recognizer {
    def apply(in: Input) = {
      fail(in)
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

  case class Seq(p: Recognizer, q: Recognizer) extends Recognizer {
    def apply(in: Input) = {
      p(in)
      q(in)
    }
  }

  case class Or(p: Recognizer, q: Recognizer) extends Recognizer {
    def apply(in: Input) = {
      val back = in.position;

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

  case class Look(p: Recognizer) extends Recognizer {
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

  case class Rep(p: Recognizer) extends Recognizer {
    val q = p.look

    def apply(in: Input) = {
      q(in)
      this(in)
    } or {
    }
  }
}