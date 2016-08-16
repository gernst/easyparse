package arse.bi

import arse._

case class Cast[A, B >: A, S](p: Rel[A, S])(implicit ev: ClassTag[A]) extends Rel[B, S] {
  override def toString = "" + p

  def parse(s: S) = {
    p parse s
  }

  def format(bs: (B, S)) = bs match {
    case (a: A, s) => p format (a, s)
    case _ => fail
  }
}