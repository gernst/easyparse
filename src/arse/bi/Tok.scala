package arse.bi

import arse._

class Tok[A]() extends Rel[A, List[A]] {
  override def toString = "."

  def parse(as: List[A]) = as match {
    case a :: as => (a, as)
    case _ => fail
  }

  def format(aas: (A, List[A])) = aas match {
    case (a, as) => as :+ a
    case _ => fail
  }
}
