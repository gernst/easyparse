package arse.bi

trait <=>[A, B] {
  def apply(a: A): B
  def unapply(b: B): A
}