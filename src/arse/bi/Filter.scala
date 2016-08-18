package arse.bi

import arse._

case class Filter[A, C, S](p: Rel[A, S], t: A => C, c: C)(implicit ev: ClassTag[A]) extends Rel[A, S] {
  override def toString = "" + p

  def parse(s0: S) = {
    val (a, s1) = p  parse s0
    if(t(a) != c) fail
    (a, s1)
  }

  def format[B >: A](bs: (B, S)) = {
    val (a: A, s) = bs
    if(t(a) != c) fail
    p format (a, s)
  }
}
