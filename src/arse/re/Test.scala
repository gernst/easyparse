package arse.re

object Test {
  def main(args: Array[String]) {
    val a = Match('a')
    val b = Match('b')
    val c = Match('c')
    // val qo = Match('“')
    // val qc = Match('”')
    // val re = a.+ ~ Group("BS", Group("B", b).*) ~ ((b ~ a ~ c) | (a ~ a ~ c))
    // val re = ((a ~ b) | (a)) ~ ((c) | (b ~ c))
    // (Group("A", a ~ b) | (a ~ b)) ~ Group("C", c) // ~ Group("B", a.*)
    val re = Lexical(Map("a" -> a, "b" -> a ~ b ~ c))
    val dfa = DFA(re)
    val scanner = Scanner(dfa)

    dfa.print
    println

    val in = "abc".getBytes
    println("dfa matched " + new String(in take (dfa ~ in)._2))
    println("scanner matched " + new String(in take (scanner ~ in)))
  }
}