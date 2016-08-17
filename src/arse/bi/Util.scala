package arse.bi

import arse._

abstract class Rel1[A1, B, C >: B, S](p: Rel[A1, S])(implicit ev: ClassTag[B]) extends Rel[C, S] with (A1 => B) {
  override def toString = ev.runtimeClass.getSimpleName + "(" + p + ")"

  def apply(a1: A1): B
  def unapply(b: B): Option[A1]

  def parse(s0: S): (C, S) = {
    val (a1, s1) = p parse s0
    (apply(a1), s1)
  }

  def format(cs: (C, S)): S = cs match {
    case (b: B, s0) =>
      unapply(b) match {
        case Some(a1) =>
          val s1 = p format (a1, s0)
          s1
        case None => fail
      }
    case _ => fail
  }
}

abstract class Rel2[A1, A2, B, C >: B, S](p: Rel[A1, S], q: Rel[A2, S])(implicit ev: ClassTag[B]) extends Rel[C, S] with ((A1, A2) => B) {
  override def toString = ev.runtimeClass.getSimpleName + "(" + p + ", " + q + ")"

  def apply(a1: A1, a2: A2): B
  def unapply(b: B): Option[(A1, A2)]

  def parse(s0: S): (C, S) = {
    val (a1, s1) = p parse s0
    val (a2, s2) = q parse s1
    (apply(a1, a2), s2)
  }

  def format(cs: (C, S)): S = cs match {
    case (b: B, s0) =>
      unapply(b) match {
        case Some((a1, a2)) =>
          val s1 = p format (a1, s0)
          val s2 = q format (a2, s1)
          s2
        case None => fail
      }
    case _ => fail
  }
}
