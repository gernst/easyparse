package arse.re

object Test {
  def main(args: Array[String]) {
    val a = Match('a')
    val b = Match('b')
    val c = Match('c')
    // val qo = Match('“')
    // val qc = Match('”')
    // val re = a.+ ~ Group("BS", Group("B", b).*) ~ ((b ~ a ~ c) | (a ~ a ~ c))
    val re = (Group("A", a ~ b) | Group("B", a)) ~ (Group("A", c) | Group("B", b ~ c))
    // (Group("A", a ~ b) | (a ~ b)) ~ Group("C", c) // ~ Group("B", a.*)
    val dfa = DFA(re)
    val scanner = Scanner(dfa)

    dfa.print
    println

    val in = "abc".getBytes
    println("dfa matched " + new String(in take (dfa ~ in)))
    println("scanner matched " + new String(in take (scanner ~ in)))
  }
}