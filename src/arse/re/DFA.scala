package arse.re

import scala.collection.immutable.BitSet

case class DFA(qs: States, init: State, d: Transitions, fin: Set[State]) {
  import DFA._

  def enter(i: Int, g: Set[String]) = {
    if (!g.isEmpty)
      println("enter " + g.mkString(" ") + " at " + i)
  }

  def leave(i: Int, g: Set[String]) = {
    if (!g.isEmpty)
      println("leave " + g.mkString(" ") + " at " + i)
  }

  def ~(cs: Iterable[Byte]): Int = {
    var q = init
    var a = 0
    // enter(0,g)

    for ((c, i) <- cs.zipWithIndex) {
      d get ((q, c)) match {
        case None =>
          return a
        case Some((gs, qc)) =>
          println(q + " -- " + Letter.fmt(c) + " --> " + qc)
          // leave(i+1, g -- gc)
          // enter(i+1, gc -- g)
          q = qc
          if (fin contains q) a = i + 1
      }
    }
    return a
  }

  def sts(qs: States) = {
    qs mkString ("{ ", ", ", " }")
  }

  def ts(t: ((State, Iterable[Letter]), (Groups, State))) = {
    val ((q0, cs), (gs, q1)) = t
    q0 + " -- " + Letters.compact(cs) + " --> " + q1 + gs.mkString(" {", ", ", "}")
  }

  def tts(d: Transitions) = {
    val gs = d.groupBy { case ((q0, _), q1) => (q0, q1) }
    val tr = gs map {
      case ((q0, q1), m) =>
        val cs = m.map(_._1._2)
        ((q0, cs), q1)
    }
    tr map ts mkString ("{\n  ", ",\n  ", "\n}")
  }

  def print = {
    println("qs = " + sts(qs))
    println("init = " + init)
    println("d = " + tts(d))
    println("fin = " + sts(fin))
  }
}

object DFA {
  def goto(q: State): (Letter, (States, Transitions)) => (States, Transitions) = {
    case (c, (qs, d)) =>
      val qc = q derive c
      val gs = q active c
      val dc = d + ((q, c) -> (gs, qc))
      if (qs contains qc) {
        (qs, dc)
      } else {
        explore(qs + qc, dc, qc)
      }
  }

  def explore(qs: States, d: Transitions, q: State): (States, Transitions) = {
    val cs = q.first
    // val cs = (0 until 256)
    cs.foldRight((qs, d))(goto(q))
  }

  def apply(init: Regex): DFA = {
    val (qs, d) = explore(Set(init), Map(), init)
    val fin = qs filter (_.isNullable)
    DFA(qs, init, d, fin)
  }
}