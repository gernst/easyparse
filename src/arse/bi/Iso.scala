package arse.bi

import arse._

// cannot use unapply, clashes with <~ and case class companions
trait <=[A, B] { def inverse(b: B): A }

trait <=>[A, B] extends (A => B) with (A <= B) {
  val ^ = ((b: B) => inverse(b))
}

trait <~[A, B] extends (A <= B) {
  def unapply(b: B): Option[A]
  def inverse(b: B) = unapply(b) getOrElse fail
}

trait <~>[A, B] extends (A <=> B) with (A <~ B)

trait Iso1[A, R] extends (A <~> R) {
  def from[S](p: Rel[A, S]): Rel[R, S] = p map this
}

trait Iso2[A, B, R] extends ((A, B) <~> R) {
  def apply(a: A, b: B): R
  def apply(p: (A, B)) = apply(p._1, p._2)
  def from[S](p: Rel[A, S], q: Rel[B, S]): Rel[R, S] = Seq(p,q) map this
}

trait Iso3[A, B, C, R] extends ((A, B, C) <~> R) {
  def apply(a: A, b: B, c: C): R
  def apply(p: (A, B, C)) = apply(p._1, p._2, p._3)
}