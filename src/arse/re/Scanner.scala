package arse.re

import scala.collection.immutable.BitSet

case class Scanner(qs: Array[State], init: Int, d: Array[Array[Int]], fin: BitSet) extends ScannerLike {
  def ~(cs: Iterable[Byte]): (State, Int) = {
    var q = init
    var a = qs(init)
    var n = 0

    for ((c, i) <- cs.zipWithIndex) {
      d(q)(Letter(c)) match {
        case 0 =>
          return (a,n)
        case qc =>
          // println(q + " -- " + Letter.fmt(c) + " --> " + qc)
          q = qc
          if (fin contains q) {
            a = qs(q)
            n = i + 1
          }
      }
    }
    return (a,n)
  }
}

object Scanner {
  def apply(dfa: DFA): Scanner = dfa match {
    case DFA(qs, init, d, fin) =>
      val n = qs.size
      // indices are 1-based
      // 0 denotes nonexistent transitions
      val index = (qs,1 to n).zipped.toMap
      val rs = new Array[State](n + 1)
      val t = new Array[Array[Int]](n + 1)
      for ((q, i) <- index) {
        rs(i) = q
        t(i) = new Array[Int](256)
        for (((q0, c), q1) <- d if q0 == q) {
          t(i)(c) = index(q1)
        }
      }
      Scanner(rs, index(init), t, BitSet.empty ++ (fin map index))
  }
}