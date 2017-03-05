package arse.bi

import arse.ll._

case class Opt[+A, S](p: Rel[A, S]) extends Rel[Option[A], S] {
  override def toString = "" + p + "?"

  def parse(s0: S): (Option[A], S) = {
    val (a, s1) = p parse s0
    (Some(a): Option[A], s1)
  } or {
    (None, s0)
  }

  def format[B >: Option[A]](bs: (B, S)): S = bs match {
    case (Some(b), s) =>
      p format (b, s)
    case (None, s) =>
      s
  }
}
