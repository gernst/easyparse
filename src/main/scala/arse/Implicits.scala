package arse

object implicits {
  implicit class Lit0(s: String) {
    def ~>[A](a: => A) = {
      s map a
    }
  }

  implicit class Parser1[A](p: Parser[A]) {
    def ~>[R](f: A => R) = {
      p map { case a => f(a) }
    }

    def flatMap[B](q: A => Parser[B]) = {
      parser.FlatMap(p, q)
    }
  }

  implicit class Apply1[A, R](f: A => R) {
    def apply(p: Parser[A]) = {
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

    def ~>[R](f: (A, B) => R) = {
      p map { case a ~ b => f(a, b) }
    }
  }

  implicit class Apply2[A, B, R](f: (A, B) => R) {
    def apply(p: Parser[A ~ B]) = {
      p map { case a ~ b => f(a, b) }
    }
  }

  implicit class Parser3[A, B, C](p: Parser[A ~ B ~ C]) {
    def ~>[R](f: (A, B, C) => R) = {
      p map { case a ~ b ~ c => f(a, b, c) }
    }
  }

  implicit class Apply3[A, B, C, R](f: (A, B, C) => R) {
    def apply[R](p: Parser[A ~ B ~ C]) = {
      p map { case a ~ b ~ c => f(a, b, c) }
    }
  }

  implicit class Parser4[A, B, C, D](p: Parser[A ~ B ~ C ~ D]) {
    def ~>[R](f: (A, B, C, D) => R) = {
      p map { case a ~ b ~ c ~ d => f(a, b, c, d) }
    }
  }

  implicit class Apply4[A, B, C, D, R](f: (A, B, C, D) => R) {
    def apply[R](p: Parser[A ~ B ~ C ~ D]) = {
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