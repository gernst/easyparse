package arse.bi

import arse._

class Lit[A](a: A) extends Rel[A, List[A]] {
  override def toString = "'" + a + "'"

  def apply(as: List[A]) = as match {
    case `a` :: as => (a, as)
    case _ => fail
  }

  def unapply(aas: (A, List[A])) = aas match {
    case (`a`, as) => as :+ a
    case _ => fail
  }
}
