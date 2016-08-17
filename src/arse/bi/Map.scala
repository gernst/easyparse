package arse.bi

case class Map[A, B, S](p: Rel[A, S], f: A <=> B) extends Rel[B, S] {
  override def toString = "" + p + "^"

  def parse(s0: S) = {
    val (a, s1) = p parse s0
    val b = f(a)
    (b, s1)
  }

  def format(as: (B, S)) = {
    val (b, s) = as
    val a = f ^ (b)
    p format (a, s)
  }
}
