package arse.bi

case class Seq[A, B, S](p: Rel[A, S], q: Rel[B, S]) extends Rel[(A, B), S] {
  override def toString = "" + p + " " + q

  def apply(s0: S) = {
    val (a, s1) = p apply s0
    val (b, s2) = q apply s1
    ((a, b), s2)
  }

  def unapply(abs: ((A, B), S)) = {
    val ((a, b), s0) = abs
    val s1 = p unapply (a, s0)
    val s2 = q unapply (b, s1)
    s2
  }
}