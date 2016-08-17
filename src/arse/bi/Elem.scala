package arse.bi

import arse._

case class Elem[S](x: S) extends Lit[List[S]] {
  override def toString = "'" + x + "'"

  def parse(s: List[S]) = s match {
    case `x` :: s => s
    case _ => fail
  }

  def format(s: List[S]) = {
    x :: s
  }
}
