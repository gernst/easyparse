// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import control._
import java.util.regex.Pattern

object recognizer {
  trait Recognizer extends WithFailure {
    p =>

    def apply(in: Input): Unit

    def ?(): Recognizer = {
      p | recognizer.Accept
    }

    def ~(q: Recognizer): Recognizer = {
      recognizer.Seq(p, q)
    }

    def ~[A](q: Parser[A]): Parser[A] = {
      parser.SeqP(p, q)
    }

    def |(q: Recognizer): Recognizer = {
      recognizer.Or(p, q)
    }

    def *(): Recognizer = {
      recognizer.Rep(p)
    }

    def +(): Recognizer = {
      p ~ p.*
    }

    def rep(sep: Recognizer): Recognizer = {
      p ~ (sep ~ p).*
    }

    def map[A](a: A): Parser[A] = {
      p ~ ret(a)
    }
  }

  case class Rec(name: String, p: () => Recognizer) extends Recognizer {
    def apply(in: Input) = {
      p()(in)
    }
  }

  case class Regex(pattern: String) extends Recognizer with WithPattern {
    def apply(in: Input) = {
      matches(in.text, in.position) match {
        case Some(next) =>
          in advanceTo matcher.end
        case None =>
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
      val back = in.position

      {
        in.position = back
        in.commit = false
        p(in)
      } or {
        in.position = back
        in.commit = false
        q(in)
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