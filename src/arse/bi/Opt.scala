package arse.bi

import arse._

case class Opt[A, S](p: Rel[A, S]) extends Rel[Option[A], S] {
  override def toString = "" + p + "?"

  def apply(s0: S): (Option[A], S) = {
    val (a, s1) = p apply s0
    (Some(a): Option[A], s1)
  } or {
    (None, s0)
  }

  def unapply(a: (Option[A], S)): S = a match {
    case (Some(a), s) =>
      p unapply (a, s)
    case (None, s) =>
      s
  }
}
