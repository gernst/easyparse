package arse.bi

import arse._

case class Tok[A](name: String = "__") extends Rel[A, List[A]] {
  override def toString = name

  def parse(as: List[A]) = as match {
    case a :: as => (a, as)
    case _ => fail
  }

  def format(aas: (A, List[A])) = aas match {
    case (a, as) => as :+ a
    case _ => fail
  }
}
