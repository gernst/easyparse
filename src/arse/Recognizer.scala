package arse

object recognizer {
  def R[T](p: => Recognizer[T])(implicit ops: Ops, name: sourcecode.Name): Recognizer[T] = {
    ops.rec(name.value, p)
  }

  implicit class BaseRecognizer[T](p: Recognizer[T])(implicit ops: Ops) {
    def seal = {
      ops.seal(p)
    }

    def ~(q: Recognizer[T]) = {
      ops.seq(p, q)
    }

    def ~[A](q: Parser[T, A]) = {
      ops.seq(p, q)
    }

    def ~!(q: Recognizer[T]) = {
      ops.seq(p, ops.commit(q))
    }

    def ~![A](q: Parser[T, A]) = {
      ops.seq(p, ops.commit(q))
    }

    def |(q: Recognizer[T]) = {
      ops.or(p, q)
    }

    def *() = {
      ops.rep(p)
    }

    def +() = {
      p ~ p.*
    }

    def ?() = {
      p | ops.accept
    }

    def rep(sep: Recognizer[T]) = {
      p ~ (sep ~ p).*
    }

    def map[A](a: A) = {
      p ~ ops.ret[T, A](a)
    }
  }
}