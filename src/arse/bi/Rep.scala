package arse.bi

import arse._

case class Rep[A, S](p: Rel[A, S], min: Int, max: Int = Int.MaxValue) extends Rel[List[A], S] {
  assert(0 <= min && min <= max)

  override def toString = {
    if (min == 0 && max == Int.MaxValue)
      "" + p + "*"
    if (min == 1 && max == Int.MaxValue)
      "" + p + "+"
    else
      "" + p + "{" + min + "," + max + "}"
  }

  def apply(s: S): (List[A], S) = {
    apply(s, 0)
  }

  def apply(s0: S, count: Int): (List[A], S) = if (count == max) {
    (Nil, s0)
  } else {
    val (a, s1) = p apply s0
    val (as, s2) = this apply (s1, count + 1) // already have an element
    (a :: as, s2)
  } or {
    if (min <= count) (Nil, s0)
    else fail
  }

  def unapply(ass: (List[A], S)): S = ass match {
    case (a :: as, s0) =>
      val s1 = p unapply (a, s0)
      val s2 = this unapply (as, s1)
      s2
    case (Nil, s0) =>
      s0
  }
}