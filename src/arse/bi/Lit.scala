package arse.bi

import arse._

class Lit[A](a: A) extends Rel[A, List[A]] {
  override def toString = "'" + a + "'"

  def parse(as: List[A]) = as match {
    case `a` :: as => (a, as)
    case _ => fail
  }

  def format(aas: (A, List[A])) = aas match {
    case (`a`, as) => as :+ a
    case _ => fail
  }
}
