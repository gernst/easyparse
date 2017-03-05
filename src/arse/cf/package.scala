package arse

import scala.annotation.tailrec

package object cf {
  def rec[T, A](p: => Parser[T, A]) = new Rec(p)
  def P[T, A](p: Parser[T, A])(implicit name: sourcecode.Name): Parser[T, A] = Rule(name.value, p)

  val Empty = Result(Nil)

  type Recognizer[T] = Parser[T, Unit]

  implicit class PairParser[T, A, B](p: Parser[T, (A, B)]) {
    def _1() = p map (_._1)
    def _2() = p map (_._2)
  }

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

    private def epsilon(p: Parser[T, A], in: Iterable[T]): (Result[A], Iterable[T]) = {
      val q = p.epsilon
      println(q)
      (q, in)
    }

    @tailrec
    private def parse(p: Parser[T, A], in: Iterable[T]): (Result[A], Iterable[T]) = {
      if (in.isEmpty) {
        println(p)
        epsilon(p, in)
      } else {
        val c = in.head
        print(c + ": ")
        println(p)
        val q = p derive c
        if (q == Empty) epsilon(p, in)
        else parse(q, in.tail)
      }
    }
  }
}