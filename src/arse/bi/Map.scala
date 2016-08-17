package arse.bi

case class Map[A, B >: A, C, S](p: Rel[A, S], f: B <=> C) extends Rel[C, S] {
  override def toString = "" + p + "^"

  def parse(s0: S) = {
    val (a, s1) = p  parse s0
    val b = f(a)
    (b, s1)
  }

  def format[D >: C](ds: (D, S)) = {
    val (d, s) = ds
    val a = (~f)(d)
    p format (a, s)
  }
}
