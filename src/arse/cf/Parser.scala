package arse.cf

import scala.collection.mutable
import scala.annotation.tailrec

trait Parser[-T, +A] extends (Iterable[T] => Iterable[A]) {
  def epsilon: Result[A]
  def derive(t: T): Parser[T, A]

  def derive(ts: Iterable[T]): Parser[T, A] = {
    ts.foldLeft(this)(_ derive _)
  }

  def apply(ts: Iterable[T]) = {
    val that = this derive ts
    that.epsilon match {
      case Result(as) =>
        as
    }
  }

  def ~[S <: T, B](that: Parser[S, B]): Parser[S, (A, B)] = (this, that) match {
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case _ => Seq(this, that)
  }

  def |[S <: T, B >: A](that: Parser[S, B]): Parser[S, B] = (this, that) match {
    case (Empty, _) => that
    case (_, Empty) => this
    case _ => Or(this, that)
  }

  def ?(): Parser[T, Option[A]] = {
    (this map ((a: A) => Some(a))) | result(None)
  }
  
  def *(): Parser[T, List[A]] = {
    Rep(this)
  }
  
  def +(): Parser[T, List[A]] = {
    this :: this.*
  }

  def map[B](f: A => B): Parser[T, B] = {
    this lift ((as: Iterable[A]) => as map f)
  }

  def filter(p: A => Boolean): Parser[T, A] = {
    this lift ((as: Iterable[A]) => as filter p)
  }

  def lift[C >: A, B](f: Iterable[C] => Iterable[B]): Parser[T, B] = this match {
    case Empty => Empty
    case _ => Lift(this, f)
  }
}

case class Result[+A](as: Iterable[A]) extends Parser[Any, A] {
  def epsilon = this
  def derive(t: Any) = Empty

  def ~[B](that: Result[B]) = {
    Result(for (a <- this.as; b <- that.as) yield (a, b))
  }

  def |[B >: A](that: Result[B]) = {
    Result(this.as ++ that.as)
  }

  def lift[B](f: Iterable[A] => Iterable[B]): Result[B] = {
    Result(f(as))
  }

  override def toString = as match {
    case Nil => "ε"
    case _ => "_"
  }
}

case class Lit[A](a: A) extends Parser[A, A] {
  def epsilon = Empty
  def derive(t: A) = {
    if (a == t) Result(Iterable(a)) else Empty
  }
  override def toString = a.toString
}

case class Match[A](p: A => Boolean) extends Parser[A, A] {
  def epsilon = Empty
  def derive(t: A) = {
    if (p(t)) Result(Iterable(t)) else Empty
  }
  override def toString = "."
}

case class Seq[-T, +A, +B](p1: Parser[T, A], p2: Parser[T, B]) extends Parser[T, (A, B)] {
  def epsilon = p1.epsilon ~ p2.epsilon
  def derive(t: T) = ((p1 derive t) ~ p2) | ((p1 epsilon) ~ (p2 derive t))
  override def toString = p1 + "" + p2
}

case class Or[-T, +A](p1: Parser[T, A], p2: Parser[T, A]) extends Parser[T, A] {
  def epsilon = p1.epsilon | p2.epsilon
  def derive(t: T) = (p1 derive t) | (p2 derive t)
  override def toString = "(" + p1 + " | " + p2 + ")"
}

case class Rep[-T, +A](p: Parser[T, A]) extends Parser[T, List[A]] {
  def epsilon = Result(Iterable(Nil))
  def derive(t: T) = (p derive t) :: this
  override def toString = p match {
    case _: Seq[_, _, _] => "(" + p + ")*"
    case _ => p + "*"
  }
}

case class Lift[-T, A, B](p: Parser[T, A], f: Iterable[A] => Iterable[B]) extends Parser[T, B] {
  def epsilon = p.epsilon lift f
  def derive(t: T) = (p derive t) lift f
  override def toString = p.toString
}

class Rec[T, A](_p: => Parser[T, A]) extends Parser[T, A] {
  lazy val p = _p

  var _epsilon: Option[Result[A]] = None
  var _derive: mutable.Map[T, Parser[T, A]] = mutable.Map()

  def epsilon = _epsilon getOrElse {
    _epsilon = Some(Empty)
    val q = p.epsilon
    _epsilon = Some(q)
    q
  }

  def derive(t: T) = _derive.get(t) getOrElse {
    _derive(t) = new Rec(p derive t)
    val q = p derive t
    _derive(t) = q
    q
  }

  override def toString = "^"
}
