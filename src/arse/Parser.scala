package arse

object ~ {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
}

trait Impl {
  def ret[A](a: A): Parser[Any, A]
  def seq[T, A, B](p: Parser[T, A], q: Parser[T, B]): Parser[T, (A, B)]
  def or[T, A](p: Parser[T, A], q: Parser[T, A]): Parser[T, A]
  def rep[T, A](p: Parser[T, A]): Parser[T, List[A]]
  def lift[T, A, B](p: Parser[T, A], f: Iterable[A] => Iterable[B]): Parser[T, B]
}

trait Parser[-T, +A] extends (Iterable[T] => Iterable[A]) {

  
}
