package arse.bi

import arse.ll._

case class Tok[A](name: String = "__")(implicit ev: ClassTag[A]) extends Rel[A, List[A]] {
  override def toString = name

  def parse(as: List[A]) = as match {
    case a :: as => (a, as)
    case _ => fail
  }

  def format[B >: A](bs: (B, List[A])) = bs match {
    case (a: A, as) => a :: as
    case _ => fail
  }
}
