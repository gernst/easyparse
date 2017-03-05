package arse.bi

import arse.ll._

case class Rep[+A, S](p: Rel[A, S], min: Int, max: Int = Int.MaxValue) extends Rel[List[A], S] {
  assert(0 <= min && min <= max)

  override def toString = {
    if (min == 0 && max == Int.MaxValue)
      "" + p + "*"
    if (min == 1 && max == Int.MaxValue)
      "" + p + "+"
    else
      "" + p + "{" + min + "," + max + "}"
  }

  def parse(s: S): (List[A], S) = {
    parse(s, 0)
  }

  def parse(s0: S, count: Int): (List[A], S) = if (count == max) {
    (Nil, s0)
  } else {
    val (a, s1) = p parse s0
    val (as, s2) = this parse (s1, count + 1)
    (a :: as, s2)
  } or {
    if (min <= count) (Nil, s0)
    else fail
  }

  def format[B >: List[A]](bs: (B, S)): S = {
    val (b: List[_], s) = bs
    format(b, s, 0)
  }

  def format(b: List[_], s0: S, count: Int): S = b match {
    case a :: as =>
      if (count == max) fail
      val s1 = this format (as, s0, count + 1)
      val s2 = p format (a, s1)
      s2
    case Nil =>
      if (min <= count) s0
      else fail
  }
}