package arse.bi

import arse._

trait Rel[A, S] {
  def parse(s: S): (A, S)
  def format(as: (A, S)): S

  def ~[B](that: Rel[B, S]) = Seq(this, that)
  def |(that: Rel[A, S])(implicit ep: ClassTag[A]) = Alt(this, that)
  def ? = Opt(this)
  def * = Rep(this, 0)
  def + = Rep(this, 1)
}

class Rec[A, S](p: => Rel[A, S]) extends Rel[A, S] {
  def parse(s: S) = p parse s
  def format(as: (A, S)) = p format as
}