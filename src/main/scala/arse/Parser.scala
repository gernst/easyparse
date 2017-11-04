// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import java.util.regex.Pattern

import control._

object parser {
  trait Parser[+A] extends WithFailure {
    p =>

    def apply(in: Input): A

    def ~(q: Recognizer): Parser[A] = {
      parser.SeqR(p, q)
    }

    def ~[B](q: Parser[B]): Parser[A ~ B] = {
      parser.Seq(p, q)
    }

    def |[B >: A](q: Parser[B]): Parser[B] = {
      parser.Or(p, q)
    }

    def *(): Parser[List[A]] = {
      parser.Rep(p)
    }

    def +(): Parser[List[A]] = {
      import implicits.ListParser
      p :: p.*
    }

    def ?(): Parser[Option[A]] = {
      (p map (Some(_))) | ret(None)
    }

    def rep(sep: Recognizer): Parser[List[A]] = {
      import implicits.ListParser
      p :: (sep ~ p).*
    }

    def ~*(sep: Recognizer): Parser[List[A]] = {
      import implicits.ListParser
      p :: (sep ~ p).*
    }

    def map[B](f: A => B): Parser[B] = {
      parser.Map(p, f)
    }

    def filter(f: A => Boolean): Parser[A] = {
      p map { case a if (f(a)) => a }
    }

    def filterNot(f: A => Boolean): Parser[A] = {
      p map { case a if (!f(a)) => a }
    }

    def reduceLeft[B >: A](f: (B, A) => B): Parser[B] = {
      p.+ map (_ reduceLeft f)
    }

    def reduceRight[B >: A](f: (A, B) => B): Parser[B] = {
      p.+ map (_ reduceRight f)
    }

    def foldLeft[B](z: => Parser[B])(f: (B, A) => B): Parser[B] = {
      (z ~ p.*) map {
        case b ~ as => as.foldLeft(b)(f)
      }
    }

    def foldRight[B](z: => Parser[B])(f: (A, B) => B): Parser[B] = {
      (p.* ~ z) map {
        case as ~ b => as.foldRight(b)(f)
      }
    }
  }

  case class Rec[A](name: String, p: () => Parser[A]) extends Parser[A] {
    def apply(in: Input) = {
      p()(in)
    }
  }

  case object Fail extends Parser[Nothing] {
    def apply(in: Input) = {
      fail(in)
    }
  }

  case class Accept[+A](a: A) extends Parser[A] {
    def apply(in: Input) = {
      a
    }
  }

  case class Regex(pattern: String) extends Parser[String] with WithPattern {
    def apply(in: Input) = {
      matches(in.text, in.position) match {
        case Some(next) =>
          in advanceTo next
          matcher.group()
        case None =>
          fail(in)
      }
    }
  }

  case class Seq[+A, +B](p: Parser[A], q: Parser[B]) extends Parser[A ~ B] {
    def apply(in: Input) = {
      val a = p(in)
      val b = q(in)
      (a, b)
    }
  }

  case class Or[+A](p: Parser[A], q: Parser[A]) extends Parser[A] {
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

  case class Rep[+A](p: Parser[A]) extends Parser[List[A]] {
    def apply(in: Input) = {
      // TODO: code this in a loop
      in.commit = false
      val a = p(in)
      val as = this(in)
      a :: as
    } or {
      Nil
    }
  }

  case class Map[A, +B](p: Parser[A], f: A => B) extends Parser[B] {
    def apply(in: Input) = {
      val a = p(in)
      try {
        f(a)
      } catch {
        case cause: Exception =>
          fail(in, cause)
      }
    }
  }

  case class FlatMap[A, B](p: Parser[A], q: A => Parser[B]) extends Parser[B] {
    def apply(in: Input) = {
      val a = p(in)
      val b = q(a)(in)
      b
    }
  }

  case class SeqP[+A](p: Recognizer, q: Parser[A]) extends Parser[A] {
    def apply(in: Input) = {
      p(in)
      val a = q(in)
      a
    }
  }

  case class SeqR[+A](p: Parser[A], q: Recognizer) extends Parser[A] {
    def apply(in: Input) = {
      val a = p(in)
      q(in)
      a
    }
  }
}