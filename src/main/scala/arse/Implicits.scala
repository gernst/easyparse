package arse

object implicits {
  implicit class Parser0[R](f: => R) {
    def apply(s: String): Parser[R] = {
      new Literal(s) map { _ => f }
    }
  }

  implicit class Apply1[A, R](f: A => R) {
    def apply(p: Parser[A]): Parser[R] = {
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
  }

  implicit class Apply2[A, B, R](f: (A, B) => R) {
    def apply(p: Parser[A ~ B]): Parser[R] = {
      p map { case a ~ b => f(a, b) }
    }
  }

  implicit class Apply3[A, B, C, R](f: (A, B, C) => R) {
    def apply(p: Parser[A ~ B ~ C]): Parser[R] = {
      p map { case a ~ b ~ c => f(a, b, c) }
    }
  }

  implicit class Apply4[A, B, C, D, R](f: (A, B, C, D) => R) {
    def apply(p: Parser[A ~ B ~ C ~ D]): Parser[R] = {
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