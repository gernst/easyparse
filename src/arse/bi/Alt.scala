package arse.bi

import arse._

case class Alt[A, S](p: Rel[A, S], q: Rel[A, S]) extends Rel[A, S] {
  override def toString = "(" + p + " | " + q + ")"

  def parse(s: S) = {
    (p parse s) or (q parse s)
  }

  def format[B >: A](bs: (B, S)) = {
    (p format bs) or (q format bs)
  }
}
