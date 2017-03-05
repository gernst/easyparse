package arse.cf

import scala.collection.mutable
import scala.annotation.tailrec

object ~ {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
}

trait Parser[-T, +A] extends (Iterable[T] => Iterable[A]) {
  def prints: Boolean
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

  def ~>[S <: T, B](that: Parser[S, B]) = (this ~ that)._2
  def <~[S <: T, B](that: Parser[S, B]) = (this ~ that)._1

  def ?() = (this map ((a: A) => Some(a))) | result(None)
  def *() = Rep(this)
  def +() = this :: this.*

  def rep[S <: T, B](sep: Parser[S, B]) = {
    this :: (sep ~> this).*
  }

  def map[B](f: A => B): Parser[T, B] = {
    this lift ((as: Iterable[A]) => as map f)
  }

  def reduceLeft[B >: A](f: (B, A) => B): Parser[T,B] = {  
    this.+ map (_ reduceLeft f)
  }

  def reduceRight[B >: A](f: (A, B) => B): Parser[T,B] = {  
    this.+ map (_ reduceRight f)
  }

  def foldLeft[S <: T, B](z: => Parser[S, B])(f: (B, A) => B): Parser[S, B] = {
    (z ~ this.*) map {
      case (b, as) => as.foldLeft(b)(f)
    }
  }

  def foldRight[S <: T, B](z: => Parser[S, B])(f: (A, B) => B): Parser[S, B] = {
    (this.* ~ z) map {
      case (as, b) => as.foldRight(b)(f)
    }
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
  def prints = false
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
    case Nil => "fail"
    case _ => as.mkString("Result(", ", ", ")")
  }
}

case class Rule[-T, +A](name: String, p: Parser[T, A]) extends Parser[T, A] {
  def prints = true
  def epsilon = p.epsilon
  def derive(t: T) = p derive t
  override def toString = name
}

case class Lit[A](a: A) extends Parser[A, A] {
  def prints = true
  def epsilon = Empty
  def derive(t: A) = {
    if (a == t) Result(Iterable(a)) else Empty
  }
  override def toString = a match {
    case c: Char => "'" + c + "'"
    case s: String => "\"" + s + "\""
    case _ => a.toString
  }
}

case class Match[A](p: A => Boolean) extends Parser[A, A] {
  def prints = true
  def epsilon = Empty
  def derive(t: A) = {
    if (p(t)) Result(Iterable(t)) else Empty
  }
  override def toString = "."
}

case class Seq[-T, +A, +B](p1: Parser[T, A], p2: Parser[T, B]) extends Parser[T, (A, B)] {
  def prints = p1.prints || p2.prints
  def epsilon = p1.epsilon ~ p2.epsilon
  def derive(t: T) = ((p1 derive t) ~ p2) | ((p1 epsilon) ~ (p2 derive t))
  override def toString = {
    if (!p1.prints) p2.toString
    else if (!p2.prints) p1.toString
    else p1 + " ~ " + p2
  }
}

case class Or[-T, +A](p1: Parser[T, A], p2: Parser[T, A]) extends Parser[T, A] {
  def prints = p1.prints || p2.prints
  def epsilon = p1.epsilon | p2.epsilon
  def derive(t: T) = (p1 derive t) | (p2 derive t)
  override def toString = {
    if (!p1.prints) p2.toString
    else if (!p2.prints) p1.toString
    else "(" + p1 + " | " + p2 + ")"
  }
}

case class Rep[-T, +A](p: Parser[T, A]) extends Parser[T, List[A]] {
  def prints = true
  def epsilon = Result(Iterable(Nil))
  def derive(t: T) = (p derive t) :: this
  override def toString = p match {
    case _: Seq[_, _, _] => "(" + p + ")*"
    case _ => p + "*"
  }
}

case class Lift[-T, A, B](p: Parser[T, A], f: Iterable[A] => Iterable[B]) extends Parser[T, B] {
  override def prints = p.prints
  def epsilon = p.epsilon lift f
  def derive(t: T) = (p derive t) lift f
  override def toString = p.toString
}

class Rec[T, A](_p: => Parser[T, A]) extends Parser[T, A] {
  def prints = true
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
    val q = rec(p derive t)
    _derive(t) = q
    q
  }

  override def toString = "^"
}
