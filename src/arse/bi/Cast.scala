package arse.bi

import arse._

case class Cast[A, B >: A, S](p: Rel[A, S])(implicit ev: ClassTag[A]) extends Rel[B, S] {
  def parse(s0: S): (A, S) = {
    p parse s0
  }

  def format(bs: (B, S)): S = bs match {
    case (a: A, s) => p format (a, s)
    case _ => fail
  }
}