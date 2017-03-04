package arse.re

object Test {
  def run(s: ScannerLike, in: String) = {
    val bs = in.getBytes
    val (q, n) = s ~ bs
    val cs = new String(bs take n)
    (q, cs)
  }

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

    val in = "abc"
    println("dfa matched " + run(dfa, in))
    println("scanner matched " + run(scanner, in))
  }
}