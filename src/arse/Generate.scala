// ARSE Parser libary
// (c) 2015 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

object Generate {
  val N = 6

  def gen(n: Int) = {
    // def parse[A1, ..., An, R](f: (A1, ..., An, B) => R)
    //                          (implicit p1: Parser[A1], ..., pn: Parser[An])
    //     = (p1 ~ ... ~ pn) map { case ((a1, ...), an) => f(a1, ..., an) }

    val is = 1 to n
    val as = is map { "a" + _ }
    val As = is map { "A" + _ }
    val ps = is map { "p" + _ }
    val Ps = is map { i => "p" + i + ": Parser[A" + i + "]" }

    val res = new StringBuilder()

    res append "def parse"
    res append As.mkString("[", ", ", ", R]")
    res append As.mkString("(f: (", ", ", ") => R)")
    res append Ps.mkString("(implicit ", ", ", ")")
    res append " = "
    res append ps.mkString("(", " ~ ", ")")
    res append " map { case "
    res append as.tail.foldLeft(as.head) { case (a1, a2) => "(" + a1 + ", " + a2 + ")" }
    res append " => "
    res append as.mkString("f(", ", ", ")")
    res append " }"
  }

  def main(args: Array[String]): Unit = {
    for(n <- 1 to N)
      println(gen(n))
  }

}
