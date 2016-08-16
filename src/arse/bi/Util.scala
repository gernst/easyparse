package arse.bi

import arse._

class Cast[A, B >: A, S](p: Rel[A, S])(implicit ev: ClassTag[A]) extends Rel[B, S] {
  def apply(s: S) = {
    p apply s
  }

  def unapply(bs: (B, S)) = bs match {
    case (a: A, s) => p unapply ((a, s))
    case _ => fail
  }
}

class InvFunction1[A1, B](f: A1 => B, g: B => Option[A1]) extends (A1 <=> B) {
  def apply(a1: A1) = f(a1)
  def unapply(b: B) = g(b) getOrElse fail
}

class InvFunction2[A1, A2, B](f: (A1, A2) => B, g: B => Option[(A1, A2)]) extends ((A1, A2) <=> B) {
  val f12 = f.tupled
  def apply(a12: (A1, A2)) = f12(a12)
  def unapply(b: B) = g(b) getOrElse fail
}

class RelFunction1[A1, B](f: A1 <=> B) {
  def from[S](p1: Rel[A1, S]): Rel[B, S] = Map(p1, f)
}

class RelFunction2[A1, A2, B](f: (A1, A2) <=> B) {
  def from[S](p1: Rel[A1, S], p2: Rel[A2, S]): Rel[B, S] = Map(Seq(p1, p2), f)
}
