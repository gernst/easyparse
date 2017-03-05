package arse.cf

object Test {
  val expr: Parser[Char, Int] = P(rec(sum | product | closed | number))

  val digit = P(Match((c: Char) => c.isDigit) map (_.asDigit))

  val number = P(digit) reduceLeft {
    (n, d) => n * 10 + d
  }

  val closed = ('(' ~> expr <~ ')')

  val product = P(expr ~ '*' ~ expr) map {
    case a ~ _ ~ b => a * b
  }

  val sum = P(expr ~ '+' ~ expr) map {
    case a ~ _ ~ b => a + b
  }

  def main(args: Array[String]) {
    val in = "12345"
    println(expr parse in)
    println(expr apply in)
  }
}