package arse.bi

import arse.ll._
import Seq._

// cannot use unapply, clashes with <~ and case class companions
trait <=[A, +B] { def inverse[C >: B](c: C): A }

trait <=>[A, +B] extends (A => B) with (A <= B) {
  val unary_~ = inverse _
}

trait <~[A, B] extends (A <= B) {
  def unapply(b: B): Option[A]
  def inverse[C >: B](c: C) = unapply(c.asInstanceOf[B]) getOrElse fail
}

trait <~>[A, B] extends (A <=> B) with (A <~ B)

trait Iso1[A1, R] extends (A1 <~> R) {
  def from[S](p: Rel[A1, S]): Rel[R, S] = p map this
}

trait Iso2[A1, A2, R] extends ((A1, A2) <~> R) {
  def apply(a1: A1, a2: A2): R
  def apply(p: (A1, A2)) = apply(p._1, p._2)
  def from[S](p1: Rel[A1, S], p2: Rel[A2, S]): Rel[R, S] = Seq(p1, p2) map this
}

trait Iso3[A1, A2, A3, R] extends ((A1, A2, A3) <~> R) {
  def apply(a1: A1, a2: A2, a3: A3): R
  def apply(p: (A1, A2, A3)) = apply(p._1, p._2, p._3)
  def from[S](p1: Rel[A1, S], p2: Rel[A2, S], p3: Rel[A3, S]): Rel[R, S] = Seq(p1, p2, p3) map this
}

trait Iso4[A1, A2, A3, A4, R] extends ((A1, A2, A3, A4) <~> R) {
  def apply(a1: A1, a2: A2, a3: A3, ad4: A4): R
  def apply(p: (A1, A2, A3, A4)) = apply(p._1, p._2, p._3, p._4)
  def from[S](p1: Rel[A1, S], p2: Rel[A2, S], p3: Rel[A3, S], p4: Rel[A4, S]): Rel[R, S] = Seq(p1, p2, p3, p4) map this
}