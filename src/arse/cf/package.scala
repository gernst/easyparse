package arse

import scala.annotation.tailrec

package object cf {
  val Empty = Result(Nil)

  implicit class ListParser[T, A](p: Parser[T, List[A]]) {
    def ::(q: Parser[T, A]) = (q ~ p) map { case (a, as) => a :: as }
    def ++(q: Parser[T, List[A]]) = (p ~ q) map { case (as, bs) => as ++ bs }
  }

  def result[A](a: A) = Result(Iterable(a))
  implicit def toLit[A](a: A) = Lit(a)

  implicit class PartialParser[T, A](p: Parser[T, A]) {
    def parse(in: Iterable[T]): (Result[A], Iterable[T]) = {
      parse(p, in)
    }

    @tailrec
    private def parse(p: Parser[T, A], in: Iterable[T]): (Result[A], Iterable[T]) = {
      if (in.isEmpty) {
        (p.epsilon, in)
      } else {
        val q = p derive in.head
        if (q == Empty) (p.epsilon, in)
        else parse(q, in.tail)
      }
    }
  }
}