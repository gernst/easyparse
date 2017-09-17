// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package object arse {
  def parens(str: String) = "(" + str + ")"

  implicit def toRecognizer[T](t: T): Recognizer[List[T]] = recognizer.Lit(t)

  def string: Parser[List[String], String] = parser.Tok()

  def int: Parser[List[String], Int] = string map {
    str => try { str.toInt } catch { case _: NumberFormatException => control.fail }
  }

  def lit[T, A](p: Recognizer[List[T]], a: A) = p map a
  def ret[S, A](a: A) = parser.Ret[S, A](a)

  def mixfix[T, O, E](p: => Parser[List[T], E],
    op: T => O,
    ap: (O, List[E]) => E,
    s: Syntax[T],
    min: Int = Int.MinValue,
    max: Int = Int.MaxValue)(implicit name: sourcecode.Name) = {
    Mixfix[List[T], O, E](name.value, () => p, ap, s prefix_op op, s postfix_op op, s infix_op op, min, max)
  }

  def mangle(s: String) = {
    s.replace('_', ' ').trim
  }

  def P[S, A](p: => Parser[S, A])(implicit name: sourcecode.Name): Parser[S, A] = {
    parser.Rec(mangle(name.value), () => p)
  }

  def R[S](p: => Recognizer[S])(implicit name: sourcecode.Name): Recognizer[S] = {
    recognizer.Rec(mangle(name.value), () => p)
  }

  trait Recognizer[S] extends (S => S) {
    p =>

    def unary_! = {
      recognizer.Commit(p)
    }

    def ~(q: Recognizer[S]): Recognizer[S] = {
      recognizer.Seq(p, q)
    }

    def ~[A](q: Parser[S, A]): Parser[S, A] = {
      parser.SeqP(p, q)
    }

    def ~!(q: Recognizer[S]): Recognizer[S] = {
      recognizer.Seq(p, !q)
    }

    def ~![A](q: Parser[S, A]): Parser[S, A] = {
      parser.SeqP(p, !q)
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

  trait Parser[S, +A] extends (S => (A, S)) {
    p =>
    // implicit class BaseParser[S, A](p: Parser[S, A]) {
    def unary_! = {
      parser.Commit(p)
    }

    def unary_? = {
      recognizer.Drop(p)
    }

    def ~(q: Recognizer[S]): Parser[S, A] = {
      parser.SeqR(p, q)
    }

    def ~[B](q: Parser[S, B]): Parser[S, A ~ B] = {
      parser.Seq(p, q)
    }

    def ~!(q: Recognizer[S]): Parser[S, A] = {
      parser.SeqR(p, !q)
    }

    def ~![B](q: Parser[S, B]): Parser[S, A ~ B] = {
      parser.Seq(p, !q)
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

    def ~*(sep: Recognizer[S]): Parser[S, List[A]] = {
      p :: (sep ~ p).*
    }

    def map[B](f: A => B): Parser[S, B] = {
      parser.Map(p, f)
    }

    def collect[B](f: PartialFunction[A, B]): Parser[S, B] = {
      filter(f.isDefinedAt) map f
    }

    def filter(f: A => Boolean): Parser[S, A] = {
      parser.Filter(p, f)
    }

    def filterNot(f: A => Boolean): Parser[S, A] = {
      parser.Filter(p, (a: A) => !f(a))
    }

    /* def flatMap[A <: A, B](q: A => Parser[S, B]) = {
      parser.FlatMap(p, q)
    } */

    def reduceLeft[B >: A](f: (B, A) => B): Parser[S, B] = {
      p.+ map (_ reduceLeft f)
    }

    def reduceRight[B >: A](f: (A, B) => B): Parser[S, B] = {
      p.+ map (_ reduceRight f)
    }

    def foldLeft[B](z: => Parser[S, B])(f: (B, A) => B): Parser[S, B] = {
      (z ~ p.*) map {
        case b ~ as => as.foldLeft(b)(f)
      }
    }

    def foldRight[B](z: => Parser[S, B])(f: (A, B) => B): Parser[S, B] = {
      (p.* ~ z) map {
        case as ~ b => as.foldRight(b)(f)
      }
    }
  }

  implicit class Parser1[S, A](p: Parser[S, A]) {
    def ^^[R](f: A => R) = {
      p map { case a => f(a) }
    }
  }

  implicit class Parser2[S, A, B](p: Parser[S, A ~ B]) {
    def _1(): Parser[S, A] = {
      p map (_._1)
    }

    def _2(): Parser[S, B] = {
      p map (_._2)
    }

    def ^^[R](f: (A, B) => R) = {
      p map { case a ~ b => f(a, b) }
    }
  }

  implicit class Parser3[S, A, B, C](p: Parser[S, A ~ B ~ C]) {
    def ^^[R](f: (A, B, C) => R) = {
      p map { case a ~ b ~ c => f(a, b, c) }
    }
  }

  implicit class Parser4[S, A, B, C, D](p: Parser[S, A ~ B ~ C ~ D]) {
    def ^^[R](f: (A, B, C, D) => R) = {
      p map { case a ~ b ~ c ~ d => f(a, b, c, d) }
    }
  }

  implicit class ListParser[S, A](p: Parser[S, List[A]]) {
    def ::(q: Parser[S, A]): Parser[S, List[A]] = {
      (q ~ p) map { case a ~ as => a :: as }
    }

    def ++(q: Parser[S, List[A]]): Parser[S, List[A]] = {
      (p ~ q) map { case as ~ bs => as ++ bs }
    }
  }
}