package arse.bi

import arse._

case class Alt[A, S](p: Rel[A, S], q: Rel[A, S]) extends Rel[A, S] {
  override def toString = "(" + p + " | " + q + ")"

  def apply(s: S) = {
    p apply s
  } or {
    q apply s
  }

  def unapply(as: (A, S)) = {
    p unapply as
  } or {
    q unapply as
  }
}
