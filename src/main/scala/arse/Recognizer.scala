// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import control._
import java.util.regex.Pattern

object recognizer {
  trait Recognizer extends WithFailure {
    p =>

    def parse(in: Input): Unit

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

    def map[A](a: A): Parser[A] = {
      p ~ ret(a)
    }
  }

  case class Rec(name: String, p: () => Recognizer) extends Recognizer {
    def parse(in: Input) = {
      p() parse in
    }
  }

  case class Regex(pattern: String) extends Recognizer with WithPattern {
    def parse(in: Input) = {
      matches(in.text, in.position) match {
        case Some(next) =>
          in advanceTo matcher.end
        case None =>
          fail(in)
      }
    }
  }

  case class Lit(token: String) extends Recognizer {
    def parse(in: Input) = {
      if (in.text.startsWith(token, in.position)) {
        in advanceBy token.length
      } else {
        fail(in)
      }
    }
  }

  case object EOF extends Recognizer {
    def parse(in: Input) = {
      if (!in.isEmpty)
        fail(in)
    }
  }

  case object Fail extends Recognizer {
    def parse(in: Input) = {
      fail(in)
    }
  }

  case object Accept extends Recognizer {
    def parse(in: Input) = {
    }
  }

  case class Drop[S, +A](p: Parser[A]) extends Recognizer {
    def parse(in: Input) = {
      p parse in
    }
  }

  case class Seq(p: Recognizer, q: Recognizer) extends Recognizer {
    def parse(in: Input) = {
      p parse in
      q parse in
    }
  }

  case class Or(p: Recognizer, q: Recognizer) extends Recognizer {
    def parse(in: Input) = {
      val back = in.position

      {
        in.position = back
        in.commit = false
        p parse in
      } or {
        in.position = back
        in.commit = false
        q parse in
      }
    }
  }

  case class Rep(p: Recognizer) extends Recognizer {
    def parse(in: Input) = {
      in.commit = false
      p parse in
      this parse in
    } or {
    }
  }
}