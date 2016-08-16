package arse.bi

case class Seq[A, B, S](p: Rel[A, S], q: Rel[B, S]) extends Rel[(A, B), S] {
  override def toString = "" + p + ", " + q

  def parse(s0: S) = {
    val (a, s1) = p parse s0
    val (b, s2) = q parse s1
    ((a, b), s2)
  }

  def format(abs: ((A, B), S)) = {
    val ((a, b), s0) = abs
    val s1 = p format (a, s0)
    val s2 = q format (b, s1)
    s2
  }
}