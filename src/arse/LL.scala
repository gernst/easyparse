package arse

import scala.annotation.tailrec

trait LL extends Ops {
  import control._

  trait Recognizer[T] extends arse.Recognizer[T] with (T => T)
  trait Parser[T, A] extends arse.Parser[T, A] with (T => (A, T))

  object recognizer {
    case class Rec[T](name: String, p: () => Recognizer[T]) extends Recognizer[T] {
      def apply(t: T) = {
        p()(t)
      }
    }

    case class Accept[T]() extends Recognizer[T] {
      def apply(t: T) = {
        t
      }
    }

    case class Commit[T](p: Recognizer[T]) extends Recognizer[T] {
      def apply(t: T) = {
        p(t) or abort(p + " failed", t)
      }
    }

    case class Seq[T](p: Recognizer[T], q: Recognizer[T]) extends Recognizer[T] {
      def apply(t0: T) = {
        val t1 = p(t0)
        val t2 = q(t1)
        t2
      }
    }

    case class Or[T](p: Recognizer[T], q: Recognizer[T]) extends Recognizer[T] {
      def apply(t: T) = {
        p(t) or q(t)
      }
    }

    case class Rep[T](p: Recognizer[T]) extends Recognizer[T] {
      def apply(t0: T) = {
        val t1 = p(t0)
        val t2 = this(t1)
        t2
      } or {
        t0
      }
    }
  }

  object parser {
    case class Rec[T, A](name: String, p: () => Parser[T, A]) extends Parser[T, A] {
      def apply(t: T) = {
        p()(t)
      }
    }

    case class Ret[T, A](a: A) extends Parser[T, A] {
      def apply(t: T) = {
        (a, t)
      }
    }

    case class Commit[T, A](p: Parser[T, A]) extends Parser[T, A] {
      def apply(t: T) = {
        p(t) or abort(p + " failed", t)
      }
    }

    case class Seq[T, A, B](p: Parser[T, A], q: Parser[T, B]) extends Parser[T, (A, B)] {
      def apply(t0: T) = {
        val (a, t1) = p(t0)
        val (b, t2) = q(t1)
        ((a, b), t2)
      }
    }

    case class Or[T, A](p: Parser[T, A], q: Parser[T, A]) extends Parser[T, A] {
      def apply(t: T) = {
        p(t) or q(t)
      }
    }

    case class Rep[T, A](p: Parser[T, A]) extends Parser[T, List[A]] {
      def apply(t0: T) = {
        val (a, t1) = p(t0)
        val (as, t2) = this(t1)
        (a :: as, t2)
      } or {
        (Nil, t0)
      }
    }

    case class Map[T, A, B](p: Parser[T, A], f: A => B) extends Parser[T, B] {
      def apply(t0: T) = {
        val (a, t1) = p(t0)
        (f(a), t1)
      }
    }

    case class Filter[T, A](p: Parser[T, A], f: A => Boolean) extends Parser[T, A] {
      def apply(t0: T) = {
        val (a, t1) = p(t0)
        if (!f(a)) fail
        (a, t1)
      }
    }

    case class SeqP[T, A](p: Recognizer[T], q: Parser[T, A]) extends Parser[T, A] {
      def apply(t0: T) = {
        val t1 = p(t0)
        val (a, t2) = q(t1)
        (a, t2)
      }
    }

    case class SeqR[T, A](p: Parser[T, A], q: Recognizer[T]) extends Parser[T, A] {
      def apply(t0: T) = {
        val (a, t1) = p(t0)
        val t2 = q(t1)
        (a, t2)
      }
    }

  }

  def rec[T](name: String, p: => Recognizer[T]) = recognizer.Rec(name, () => p)
  def accept[T] = recognizer.Accept()
  def commit[T](p: Recognizer[T]) = recognizer.Commit(p)
  def seq[T](p: Recognizer[T], q: Recognizer[T]) = recognizer.Seq(p, q)
  def or[T](p: Recognizer[T], q: Recognizer[T]) = recognizer.Or(p, q)
  def rep[T](p: Recognizer[T]) = recognizer.Rep(p)

  def rec[T, A](name: String, p: => Parser[T, A]) = parser.Rec(name, () => p)
  def ret[T, A](a: A) = parser.Ret(a)
  def commit[T, A](p: Parser[T, A]) = parser.Commit(p)
  def seq[T, A, B](p: Parser[T, A], q: Parser[T, B]) = parser.Seq(p, q)
  def or[T, A](p: Parser[T, A], q: Parser[T, A]) = parser.Or(p, q)
  def rep[T, A](p: Parser[T, A]) = parser.Rep(p)
  def map[T, A, B](p: Parser[T, A], f: A => B) = parser.Map(p, f)
  def filter[T, A](p: Parser[T, A], f: A => Boolean) = parser.Filter(p, f)

  def seq[T, A](p: Parser[T, A], q: Recognizer[T]) = parser.SeqR(p, q)
  def seq[T, A](p: Recognizer[T], q: Parser[T, A]) = parser.SeqP(p, q)

  def seal[T](p: Recognizer[T]) = p

  def seal[T, A](p: Parser[T, A]) = p
}