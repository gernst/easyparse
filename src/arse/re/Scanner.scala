package arse.re

import scala.collection.immutable.BitSet

case class Scanner(init: Int, d: Array[Array[Int]], fin: BitSet) {
  def ~(cs: Iterable[Byte]): Int = {
    var q = init
    var n = 0

    for ((c, i) <- cs.zipWithIndex) {
      d(q)(c) match {
        case 0 =>
          return n
        case qc =>
          // println(q + " -- " + Letter.fmt(c) + " --> " + qc)
          q = qc
          if (fin contains q) n = i + 1
      }
    }
    return n
  }
}

object Scanner {
  def apply(dfa: DFA): Scanner = dfa match {
    case DFA(qs, init, d, fin) =>
      val n = qs.size
      // indices are 1-based
      // 0 denotes nonexistent transitions
      val index = (qs,1 to n).zipped.toMap
      val t = new Array[Array[Int]](n + 1)
      for ((q, i) <- index) {
        t(i) = new Array[Int](256)
        for (((q0, c), (_, _, q1)) <- d if q0 == q) {
          t(i)(c) = index(q1)
        }
      }
      Scanner(index(init), t, BitSet.empty ++ (fin map index))
  }
}