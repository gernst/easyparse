package arse

trait Recognizer[T] {
  def seal: (T => T)
}

object recognizer {
  implicit class BaseRecognizer[T](p: Recognizer[T])(implicit ops: Ops) {
    def ~(q: Recognizer[T]) = {
      ops.seq(p, q)
    }
    
    def ~[B](q: Parser[T, B]) = {
      ops.seq(p, q)
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
  }
}