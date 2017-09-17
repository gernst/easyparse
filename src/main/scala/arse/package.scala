// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package object arse {
  type ~[+A, +B] = Tuple2[A, B]
  val ~ = Tuple2

  class Input(val text: String, var position: Int) {
    def length = text.length - position
    def isEmpty = (length == 0)
  }

  val accept = recognizer.Accept
  def ret[A](a: A) = parser.Accept(a)

  implicit def lit(text: String) = recognizer.Lit(text)
  def lit[A](text: String, a: A) = recognizer.Lit(text) map a

  def scan(pattern: String) = parser.Regex(pattern)

  def int = scan("[+-]?[0-9]+") map {
    str =>
      import control._
      str.toInt.mask[NumberFormatException]
  }

  def char = scan("\'([^\']|\\')\'") map {
    str =>
      str.substring(1, str.length - 1)
  }

  def string = scan("\"[^\"]*\"") map {
    str =>
      str.substring(1, str.length - 1)
  }

  /*def mixfix[O, E](p: => Parser[S,E],
    op: T => O,
    ap: (O, List[E]) => E,
    s: Syntax[T],
    min: Int = Int.MinValue,
    max: Int = Int.MaxValue)(implicit name: sourcecode.Name) = {
    Mixfix[List[T], O, E](name.value, () => p, ap, s prefix_op op, s postfix_op op, s infix_op op, min, max)
  }*/

  def P[A](p: => Parser[A])(implicit name: sourcecode.Name): Parser[A] = {
    parser.Rec(name.value, () => p)
  }

  def R(p: => Recognizer)(implicit name: sourcecode.Name): Recognizer = {
    recognizer.Rec(name.value, () => p)
  }

  trait Whitespace extends (Input => Unit) {
    p =>
  }

  trait Recognizer extends (Input => Unit) {
    p =>

    def unary_! = {
      recognizer.Commit(p)
    }

    def ~(q: Recognizer): Recognizer = {
      recognizer.Seq(p, q)
    }

    def ~[A](q: Parser[A]): Parser[A] = {
      parser.SeqP(p, q)
    }

    def ~!(q: Recognizer): Recognizer = {
      recognizer.Seq(p, !q)
    }

    def ~![A](q: Parser[A]): Parser[A] = {
      parser.SeqP(p, !q)
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

    def ?(): Recognizer = {
      p | recognizer.Accept
    }

    def rep(sep: Recognizer): Recognizer = {
      p ~ (sep ~ p).*
    }

    def map[A](a: A): Parser[A] = {
      p ~ ret(a)
    }
  }

  trait Parser[+A] extends (Input => A) {
    p =>

    def unary_! = {
      parser.Commit(p)
    }

    def unary_? = {
      recognizer.Drop(p)
    }

    def ~(q: Recognizer): Parser[A] = {
      parser.SeqR(p, q)
    }

    def ~[B](q: Parser[B]): Parser[A ~ B] = {
      parser.Seq(p, q)
    }

    def ~!(q: Recognizer): Parser[A] = {
      parser.SeqR(p, !q)
    }

    def ~![B](q: Parser[B]): Parser[A ~ B] = {
      parser.Seq(p, !q)
    }

    def |[B >: A](q: Parser[B]): Parser[B] = {
      parser.Or(p, q)
    }

    def *(): Parser[List[A]] = {
      parser.Rep(p)
    }

    def +(): Parser[List[A]] = {
      p :: p.*
    }

    def ?(): Parser[Option[A]] = {
      (p map (Some(_))) | ret(None)
    }

    def rep(sep: Recognizer): Parser[List[A]] = {
      p :: (sep ~ p).*
    }

    def ~*(sep: Recognizer): Parser[List[A]] = {
      p :: (sep ~ p).*
    }

    def map[B](f: A => B): Parser[B] = {
      parser.Map(p, f)
    }

    def collect[B](f: PartialFunction[A, B]): Parser[B] = {
      p map { f applyOrElse (_, control.fail) }
    }

    def collect[B](f: A => Option[B]): Parser[B] = {
      p map { f(_) getOrElse control.fail }
    }

    def filter(f: A => Boolean): Parser[A] = {
      p map { a => if (!f(a)) control.fail; a }
    }

    def filterNot(f: A => Boolean): Parser[A] = {
      p map { a => if (f(a)) control.fail; a }
    }

    /* def flatMap[A <: A, B](q: A => Parser[B]) = {
      parser.FlatMap(p, q)
    } */

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

  implicit class Parser1[A](p: Parser[A]) {
    def ^^[R](f: A => R) = {
      p map { case a => f(a) }
    }
  }

  implicit class Parser2[A, B](p: Parser[A ~ B]) {
    def _1(): Parser[A] = {
      p map (_._1)
    }

    def _2(): Parser[B] = {
      p map (_._2)
    }

    def ^^[R](f: (A, B) => R) = {
      p map { case a ~ b => f(a, b) }
    }
  }

  implicit class Parser3[A, B, C](p: Parser[A ~ B ~ C]) {
    def ^^[R](f: (A, B, C) => R) = {
      p map { case a ~ b ~ c => f(a, b, c) }
    }
  }

  implicit class Parser4[A, B, C, D](p: Parser[A ~ B ~ C ~ D]) {
    def ^^[R](f: (A, B, C, D) => R) = {
      p map { case a ~ b ~ c ~ d => f(a, b, c, d) }
    }
  }

  implicit class ListParser[A](p: Parser[List[A]]) {
    def ::(q: Parser[A]): Parser[List[A]] = {
      (q ~ p) map { case a ~ as => a :: as }
    }

    def ++(q: Parser[List[A]]): Parser[List[A]] = {
      (p ~ q) map { case as ~ bs => as ++ bs }
    }
  }
}