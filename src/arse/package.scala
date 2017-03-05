

package object arse {
  def parens(str: String) = "(" + str + ")"
  
  trait Recognizer[T] extends (T => T) {
    def format: String
    override def toString = format
  }

  trait Parser[T, +A] extends (T => (A, T)) {
    def format: String
    override def toString = format
  }

  def P[T, A](p: => Parser[T, A])(implicit name: sourcecode.Name): Parser[T, A] = {
    parser.Rec(name.value, () => p)
  }

  def R[T](p: => Recognizer[T])(implicit name: sourcecode.Name): Recognizer[T] = {
    recognizer.Rec(name.value, () => p)
  }

  implicit class BaseRecognizer[T](p: Recognizer[T]) {
    def ~(q: Recognizer[T]): Recognizer[T] = {
      recognizer.Seq(p, q)
    }

    def ~[A](q: Parser[T, A]): Parser[T, A] = {
      parser.SeqP(p, q)
    }

    def ~!(q: Recognizer[T]): Recognizer[T] = {
      recognizer.Seq(p, recognizer.Commit(q))
    }

    def ~![A](q: Parser[T, A]): Parser[T, A] = {
      parser.SeqP(p, parser.Commit(q))
    }

    def |(q: Recognizer[T]): Recognizer[T] = {
      recognizer.Or(p, q)
    }

    def *(): Recognizer[T] = {
      recognizer.Rep(p)
    }

    def +(): Recognizer[T] = {
      p ~ p.*
    }

    def ?(): Recognizer[T] = {
      p | recognizer.Accept()
    }

    def rep(sep: Recognizer[T]): Recognizer[T] = {
      p ~ (sep ~ p).*
    }

    def map[A](a: A): Parser[T, A] = {
      p ~ parser.Ret[T, A](a)
    }
  }

  implicit class BaseParser[T, A](p: Parser[T, A]) {
    def ~(q: Recognizer[T]): Parser[T, A] = {
      parser.SeqR(p, q)
    }

    def ~[B](q: Parser[T, B]): Parser[T, (A, B)] = {
      parser.Seq(p, q)
    }

    def ~!(q: Recognizer[T]): Parser[T, A] = {
      parser.SeqR(p, recognizer.Commit(q))
    }

    def ~![B](q: Parser[T, B]): Parser[T, (A, B)] = {
      parser.Seq(p, parser.Commit(q))
    }

    def |[B >: A](q: Parser[T, B]): Parser[T, B] = {
      parser.Or(p, q)
    }

    def *(): Parser[T, List[A]] = {
      parser.Rep(p)
    }

    def +(): Parser[T, List[A]] = {
      p :: p.*
    }

    def ?(): Parser[T, Option[A]] = {
      (p map (Some(_))) | parser.Ret(None)
    }

    def rep(sep: Recognizer[T]): Parser[T, List[A]] = {
      p :: (sep ~ p).*
    }

    def map[B](f: A => B): Parser[T, B] = {
      parser.Map(p, f)
    }

    def filter(f: A => Boolean): Parser[T, A] = {
      parser.Filter(p, f)
    }

    def reduceLeft[B >: A](f: (B, A) => B): Parser[T, B] = {
      p.+ map (_ reduceLeft f)
    }

    def reduceRight[B >: A](f: (A, B) => B): Parser[T, B] = {
      p.+ map (_ reduceRight f)
    }

    def foldLeft[B](z: => Parser[T, B])(f: (B, A) => B): Parser[T, B] = {
      (z ~ p.*) map {
        case (b, as) => as.foldLeft(b)(f)
      }
    }

    def foldRight[B](z: => Parser[T, B])(f: (A, B) => B): Parser[T, B] = {
      (p.* ~ z) map {
        case (as, b) => as.foldRight(b)(f)
      }
    }
  }

  implicit class PairParser[T, A, B](p: Parser[T, (A, B)]) {
    def _1(): Parser[T, A] = {
      p map (_._1)
    }

    def _2(): Parser[T, B] = {
      p map (_._2)
    }
  }

  implicit class ListParser[T, A](p: Parser[T, List[A]]) {
    def ::(q: Parser[T, A]): Parser[T, List[A]] = {
      (q ~ p) map { case (a, as) => a :: as }
    }

    def ++(q: Parser[T, List[A]]): Parser[T, List[A]] = {
      (p ~ q) map { case (as, bs) => as ++ bs }
    }
  }
}