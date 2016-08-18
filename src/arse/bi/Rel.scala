package arse.bi

import arse._

trait Lit[S] {
  def parse(s: S): S
  def format(s: S): S

  def ~[A](that: Rel[A, S]) = Seq(this, that)
}

trait Rel[+A, S] {
  def parse(s: S): (A, S)
  def format[B >: A](bs: (B, S)): S
  
  // def ~[B](that: Rel[B, S]) = Seq(this, that)
  def |[B >: A](that: Rel[B, S]) = Alt(this, that)
  def ? = Opt(this)
  def * = Rep(this, 0)
  def + = Rep(this, 1)

  def filter[B >: A](p: B => Boolean)(implicit ev: ClassTag[B]) = Filter[B, Boolean, S](this, p, true)
  def filterNot[B >: A](p: B => Boolean)(implicit ev: ClassTag[B]) = Filter[B, Boolean, S](this, p, false)

  def map[B >: A, C](f: B <=> C): Rel[C, S] = Map(this, f)

  def ~[A](that: Lit[S]) = Seq(this, that)
}

class Rec[A, S](p: => Rel[A, S], name: String = "...") extends Rel[A, S] {
  override def toString = name

  def parse(s: S) = p parse s
  def format[B >: A](bs: (B, S)) = p format bs
}