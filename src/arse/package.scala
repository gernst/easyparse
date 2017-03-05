

package object arse {
  implicit class BaseParser[T, A](p: Parser[T, A])(implicit impl: Impl) {
    def ~[S <: T, B](q: Parser[S, B])(implicit impl: Impl) = {
      impl.seq(p, q)
    }

    def |[S <: T, B >: A](q: Parser[S, B]) = {
      impl.or(p, q)
    }

    def * = {
      impl.rep(p)
    }

    def + = {
      p :: p.*
    }

    def ? = {
      (p map ((a: A) => Some(a))) | impl.ret(None)
    }

    def ~>[S <: T, B](q: Parser[S, B]) = {
      (p ~ q)._2
    }

    def <~[S <: T, B](q: Parser[S, B]) = {
      (p ~ q)._1
    }

    def rep[S <: T, B](sep: Parser[S, B]) = {
      p :: (sep ~> p).*
    }

    def map[B](f: A => B) = {
      impl.lift(p, (as: Iterable[A]) => as map f)
    }

    def filter(f: A => Boolean) = {
      impl.lift(p, (as: Iterable[A]) => as filter f)
    }

    def reduceLeft[B >: A](f: (B, A) => B) = {
      p.+ map (_ reduceLeft f)
    }

    def reduceRight[B >: A](f: (A, B) => B) = {
      p.+ map (_ reduceRight f)
    }

    def foldLeft[S <: T, B](z: => Parser[S, B])(f: (B, A) => B) = {
      (z ~ p.*) map {
        case (b, as) => as.foldLeft(b)(f)
      }
    }

    def foldRight[S <: T, B](z: => Parser[S, B])(f: (A, B) => B) = {
      (p.* ~ z) map {
        case (as, b) => as.foldRight(b)(f)
      }
    }
  }

  implicit class PairParser[T, A, B](p: Parser[T, (A, B)])(implicit impl: Impl) {
    def _1() = p map (_._1)
    def _2() = p map (_._2)
  }

  implicit class ListParser[T, A](p: Parser[T, List[A]])(implicit impl: Impl) {
    def ::(q: Parser[T, A]) = {
      (q ~ p) map { case (a, as) => a :: as }
    }

    def ++(q: Parser[T, List[A]]) = {
      (p ~ q) map { case (as, bs) => as ++ bs }
    }
  }
}