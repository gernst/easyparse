

package object arse {
  trait Ops {
    def rec[T](name: String, p: => Recognizer[T]): Recognizer[T]
    def accept[T]: Recognizer[T]
    def seq[T](p: Recognizer[T], q: Recognizer[T]): Recognizer[T]
    def or[T](p: Recognizer[T], q: Recognizer[T]): Recognizer[T]
    def rep[T](p: Recognizer[T]): Recognizer[T]

    def rec[T, A](name: String, p: => Parser[T, A]): Parser[T, A]
    def ret[T, A](a: A): Parser[T, A]
    def seq[T, A, B](p: Parser[T, A], q: Parser[T, B]): Parser[T, (A, B)]
    def or[T, A](p: Parser[T, A], q: Parser[T, A]): Parser[T, A]
    def rep[T, A](p: Parser[T, A]): Parser[T, List[A]]
    def map[T, A, B](p: Parser[T, A], f: A => B): Parser[T, B]
    def filter[T, A](p: Parser[T, A], f: A => Boolean): Parser[T, A]

    def seq[T, A](p: Parser[T, A], q: Recognizer[T]): Parser[T, A]
    def seq[T, A](p: Recognizer[T], q: Parser[T, A]): Parser[T, A]
  }
}