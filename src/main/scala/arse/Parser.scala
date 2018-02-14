// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import java.util.regex.Pattern

import control._

object parser {
  trait Parser[+A] extends WithFailure {
    p =>

    def parse(in: Input): A

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
    def parse(in: Input) = {
      p() parse in
    }
  }

  case object Fail extends Parser[Nothing] {
    def parse(in: Input) = {
      fail(in)
    }
  }

  case class Accept[+A](a: A) extends Parser[A] {
    def parse(in: Input) = {
      a
    }
  }

  case class Regex(pattern: String) extends Parser[String] with WithPattern {
    def parse(in: Input) = {
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
    def parse(in: Input) = {
      val a = p parse in
      val b = q parse in
      (a, b)
    }
  }

  case class Or[+A](p: Parser[A], q: Parser[A]) extends Parser[A] {
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

  case class Rep[+A](p: Parser[A]) extends Parser[List[A]] {
    def parse(in: Input) = {
      val back = in.position

      {
        in.commit = false
        val a = p parse in
        val as = this parse in
        a :: as
      } or {
        in.position = back
        Nil
      }
    }
  }

  case class Map[A, +B](p: Parser[A], f: A => B) extends Parser[B] {
    def parse(in: Input) = {
      val a = p parse in
      try {
        f(a)
      } catch {
        case cause: Exception =>
          fail(in, cause)
      }
    }
  }

  case class FlatMap[A, B](p: Parser[A], q: A => Parser[B]) extends Parser[B] {
    def parse(in: Input) = {
      val a = p parse in
      val b = q(a) parse in
      b
    }
  }

  case class SeqP[+A](p: Recognizer, q: Parser[A]) extends Parser[A] {
    def parse(in: Input) = {
      p parse in
      val a = q parse in
      a
    }
  }

  case class SeqR[+A](p: Parser[A], q: Recognizer) extends Parser[A] {
    def parse(in: Input) = {
      val a = p parse in
      q parse in
      a
    }
  }
}