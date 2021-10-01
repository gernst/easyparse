// ARSE Parser libary
// (c) 2020 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

object implicits {
  implicit class Parser0[R, T](f: => R) {
    def apply(t: T): Parser[R, T] = {
      new Literal(t) map { _ => f }
    }
  }

  implicit class Apply1[A, R, T](f: A => R) {
    def apply(p: Parser[A, T]): Parser[R, T] = {
      p map { case a => f(a) }
    }
  }

  implicit class Parser2[A, B, T](p: Parser[A ~ B, T]) {
    def _1(): Parser[A, T] = {
      p map (_._1)
    }

    def _2(): Parser[B, T] = {
      p map (_._2)
    }
  }

  implicit class Apply2[A, B, R, T](f: (A, B) => R) {
    def apply(p: Parser[A ~ B, T]): Parser[R, T] = {
      p map { case a ~ b => f(a, b) }
    }
  }

  implicit class Apply3[A, B, C, R, T](f: (A, B, C) => R) {
    def apply(p: Parser[A ~ B ~ C, T]): Parser[R, T] = {
      p map { case a ~ b ~ c => f(a, b, c) }
    }
  }

  implicit class Apply4[A, B, C, D, R, T](f: (A, B, C, D) => R) {
    def apply(p: Parser[A ~ B ~ C ~ D, T]): Parser[R, T] = {
      p map { case a ~ b ~ c ~ d => f(a, b, c, d) }
    }
  }

  implicit class Apply5[A, B, C, D, E, R, T](f: (A, B, C, D, E) => R) {
    def apply(p: Parser[A ~ B ~ C ~ D ~ E, T]): Parser[R, T] = {
      p map { case a ~ b ~ c ~ d ~ e => f(a, b, c, d, e) }
    }
  }

  implicit class Apply6[A, B, C, D, E, F, R, T](g: (A, B, C, D, E, F) => R) {
    def apply(p: Parser[A ~ B ~ C ~ D ~ E ~ F, T]): Parser[R, T] = {
      p map { case a ~ b ~ c ~ d ~ e ~ f => g(a, b, c, d, e, f) }
    }
  }

  implicit class ListParser[A, T](p: Parser[List[A], T]) {
    def ::(q: Parser[A, T]): Parser[List[A], T] = {
      (q ~ p) map { case a ~ as => a :: as }
    }

    def ++(q: Parser[List[A], T]): Parser[List[A], T] = {
      (p ~ q) map { case as ~ bs => as ++ bs }
    }
  }
}
