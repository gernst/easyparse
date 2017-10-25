// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import control._
import java.util.regex.Pattern

object recognizer {
  case class Rec(name: String, p: () => Recognizer) extends Recognizer {
    def apply(in: Input) = {
      p()(in)
    }
  }

  case class Regex(pattern: String) extends Recognizer {
    val regex = Pattern.compile(pattern)
    val matcher = regex.matcher("")

    def apply(in: Input) = {
      matcher.reset(in.text)

      if (matcher.find(in.position)) {
        in advanceTo matcher.end
      } else {
        fail(in)
      }
    }
  }

  case class Lit(token: String) extends Recognizer {
    def apply(in: Input) = {
      if (in.text.startsWith(token, in.position)) {
        in advanceBy token.length
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
        in.commit = false
        p(in)
      } or {
        in.position = back
        in.commit = false
        q(in)
      } rollback {
        in.position = back
        in.commit = true
      }
    }
  }

  case class Rep(p: Recognizer) extends Recognizer {
    def apply(in: Input) = {
      in.commit = false
      p(in)
      this(in)
    } or {
    }
  }
}