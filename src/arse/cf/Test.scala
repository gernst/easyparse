package arse.cf

object Test {
  val x: Lit[Char] = 'x'
  val y: Lit[Char] = 'y'
  val r: Parser[Char, List[Char]] = new Rec(e)
  val e: Parser[Char, List[Char]] = (x :: r) | y.+

  def main(args: Array[String]) {
    println(r parse "xxxyz")
  }
}