package arse

object ~ {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = {
    Some(p)
  }
}

object parser {
  def P[T, A](p: => Parser[T, A])(implicit ops: Ops, name: sourcecode.Name): Parser[T, A] = {
    ops.rec(name.value, p)
  }

  implicit class BaseParser[T, A](p: Parser[T, A])(implicit ops: Ops) {
    def seal = {
      ops.seal(p)
    }

    def ~(q: Recognizer[T]) = {
      ops.seq(p, q)
    }

    def ~[B](q: Parser[T, B]) = {
      ops.seq(p, q)
    }
    
    def ~!(q: Recognizer[T]) = {
      ops.seq(p, ops.commit(q))
    }

    def ~![B](q: Parser[T, B]) = {
      ops.seq(p, ops.commit(q))
    }

    def |[B >: A](q: Parser[T, B]) = {
      ops.or(p, q)
    }

    def *() = {
      ops.rep(p)
    }

    def +() = {
      p :: p.*
    }

    def ?() = {
      (p map (Some(_))) | ops.ret(None)
    }

    def rep(sep: Recognizer[T]) = {
      import recognizer._
      p :: (sep ~ p).*
    }

    def map[B](f: A => B) = {
      ops.map(p, f)
    }

    def filter(f: A => Boolean) = {
      ops.filter(p, f)
    }

    def reduceLeft[B >: A](f: (B, A) => B) = {
      p.+ map (_ reduceLeft f)
    }

    def reduceRight[B >: A](f: (A, B) => B) = {
      p.+ map (_ reduceRight f)
    }

    def foldLeft[B](z: => Parser[T, B])(f: (B, A) => B) = {
      (z ~ p.*) map {
        case (b, as) => as.foldLeft(b)(f)
      }
    }

    def foldRight[B](z: => Parser[T, B])(f: (A, B) => B) = {
      (p.* ~ z) map {
        case (as, b) => as.foldRight(b)(f)
      }
    }
  }

  implicit class PairParser[T, A, B](p: Parser[T, (A, B)])(implicit ops: Ops) {
    def _1() = p map (_._1)
    def _2() = p map (_._2)
  }

  implicit class ListParser[T, A](p: Parser[T, List[A]])(implicit ops: Ops) {
    def ::(q: Parser[T, A]) = {
      (q ~ p) map { case (a, as) => a :: as }
    }

    def ++(q: Parser[T, List[A]]) = {
      (p ~ q) map { case (as, bs) => as ++ bs }
    }
  }
}