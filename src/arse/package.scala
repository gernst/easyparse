// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package object arse {
  def parens(str: String) = "(" + str + ")"

  trait Seq

  trait Recognizer[S] extends (S => S) {
    def format: String
    override def toString = format
  }

  trait Parser[S, +A] extends (S => (A, S)) {
    def format: String
    override def toString = format
  }

  def P[S, A](p: => Parser[S, A])(implicit name: sourcecode.Name): Parser[S, A] = {
    parser.Rec(name.value, () => p)
  }

  def R[S](p: => Recognizer[S])(implicit name: sourcecode.Name): Recognizer[S] = {
    recognizer.Rec(name.value, () => p)
  }

  implicit class BaseRecognizer[S](p: Recognizer[S]) {
    def ~(q: Recognizer[S]): Recognizer[S] = {
      recognizer.Seq(p, q)
    }

    def ~[A](q: Parser[S, A]): Parser[S, A] = {
      parser.SeqP(p, q)
    }

    def ~!(q: Recognizer[S]): Recognizer[S] = {
      recognizer.Seq(p, recognizer.Commit(q))
    }

    def ~![A](q: Parser[S, A]): Parser[S, A] = {
      parser.SeqP(p, parser.Commit(q))
    }

    def |(q: Recognizer[S]): Recognizer[S] = {
      recognizer.Or(p, q)
    }

    def *(): Recognizer[S] = {
      recognizer.Rep(p)
    }

    def +(): Recognizer[S] = {
      p ~ p.*
    }

    def ?(): Recognizer[S] = {
      p | recognizer.Accept()
    }

    def rep(sep: Recognizer[S]): Recognizer[S] = {
      p ~ (sep ~ p).*
    }

    def map[A](a: A): Parser[S, A] = {
      p ~ parser.Ret[S, A](a)
    }
  }

  implicit class BaseParser[S, A](p: Parser[S, A]) {
    def ~(q: Recognizer[S]): Parser[S, A] = {
      parser.SeqR(p, q)
    }

    def ~[B](q: Parser[S, B]): Parser[S, (A, B)] = {
      parser.Seq(p, q)
    }

    def ~!(q: Recognizer[S]): Parser[S, A] = {
      parser.SeqR(p, recognizer.Commit(q))
    }

    def ~![B](q: Parser[S, B]): Parser[S, (A, B)] = {
      parser.Seq(p, parser.Commit(q))
    }

    def |[B >: A](q: Parser[S, B]): Parser[S, B] = {
      parser.Or(p, q)
    }

    def *(): Parser[S, List[A]] = {
      parser.Rep(p)
    }

    def +(): Parser[S, List[A]] = {
      p :: p.*
    }

    def ?(): Parser[S, Option[A]] = {
      (p map (Some(_))) | parser.Ret(None)
    }

    def rep(sep: Recognizer[S]): Parser[S, List[A]] = {
      p :: (sep ~ p).*
    }

    def map[B](f: A => B): Parser[S, B] = {
      parser.Map(p, f)
    }

    def filter(f: A => Boolean): Parser[S, A] = {
      parser.Filter(p, f)
    }

    def reduceLeft[B >: A](f: (B, A) => B): Parser[S, B] = {
      p.+ map (_ reduceLeft f)
    }

    def reduceRight[B >: A](f: (A, B) => B): Parser[S, B] = {
      p.+ map (_ reduceRight f)
    }

    def foldLeft[B](z: => Parser[S, B])(f: (B, A) => B): Parser[S, B] = {
      (z ~ p.*) map {
        case (b, as) => as.foldLeft(b)(f)
      }
    }

    def foldRight[B](z: => Parser[S, B])(f: (A, B) => B): Parser[S, B] = {
      (p.* ~ z) map {
        case (as, b) => as.foldRight(b)(f)
      }
    }
  }

  implicit class PairParser[S, A, B](p: Parser[S, (A, B)]) {
    def _1(): Parser[S, A] = {
      p map (_._1)
    }

    def _2(): Parser[S, B] = {
      p map (_._2)
    }
  }

  implicit class ListParser[S, A](p: Parser[S, List[A]]) {
    def ::(q: Parser[S, A]): Parser[S, List[A]] = {
      (q ~ p) map { case (a, as) => a :: as }
    }

    def ++(q: Parser[S, List[A]]): Parser[S, List[A]] = {
      (p ~ q) map { case (as, bs) => as ++ bs }
    }
  }

  implicit class ParseFunction1[A1, B](f: (A1) => B)(implicit name: sourcecode.Name) {
    def from[S](p1: Parser[S, A1]): Parser[S, B] = {
      P(parser.Seq1(p1, f))(name)
    }
  }

  implicit class ParseFunction2[A1, A2, B](f: (A1, A2) => B)(implicit name: sourcecode.Name) {
    def from[S](p1: Parser[S, A1], p2: Parser[S, A2]): Parser[S, B] = {
      P(parser.Seq2(p1, p2, f))(name)
    }
  }
  implicit class ParseFunction3[A1, A2, A3, B](f: (A1, A2, A3) => B)(implicit name: sourcecode.Name) {
    def from[S](p1: Parser[S, A1], p2: Parser[S, A2], p3: Parser[S, A3]): Parser[S, B] = {
      P(parser.Seq3(p1, p2, p3, f))(name)
    }
  }

  implicit class ParseFunction4[A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => B)(implicit name: sourcecode.Name) {
    def from[S](p1: Parser[S, A1], p2: Parser[S, A2], p3: Parser[S, A3], p4: Parser[S, A4]): Parser[S, B] = {
      P(parser.Seq4(p1, p2, p3, p4, f))(name)
    }
  }
}