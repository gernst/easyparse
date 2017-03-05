package arse.bi

import arse.ll._

case class Eof[S]() extends Lit[List[S]] {
  override def toString = "$"
  
  def parse(s: List[S]) = s match {
    case Nil => Nil
    case _ => fail
  }

  def format(s: List[S]) = s match {
    case Nil => Nil
    case _ => fail
  }
}