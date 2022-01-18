// ARSE Parser libary
// (c) 2022 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import arse.{Input, Result}

trait Token

trait Scanner[T] {
  p =>

  def scan(in: Input[T], cm: Boolean): Input[T]

  def ~[B](q: Parser[B, T]): Parser[B, T] =
    new Sequence.ScannerParser(p, q, strict = true)
  def ~(q: Scanner[T]): Scanner[T] =
    new Sequence.ScannerScanner(p, q, strict = true)

  def ?~[B](q: Parser[B, T]): Parser[B, T] =
    new Sequence.ScannerParser(p, q, strict = false)
  def ?~(q: Scanner[T]): Scanner[T] =
    new Sequence.ScannerScanner(p, q, strict = false)
}

object Scanner {
  case class Keyword(name: String) extends Token with Scanner[Token] {
    def scan(in: Input[Token], cm: Boolean) = {
      if (in.nonEmpty) {
        val head = in.head
        if (head == this)
          in.tail
        else
          fail("expected keyword: '" + name + "'", in, cm)
      } else {
        fail("unexpected end of input", in, cm)
      }
    }
  }

  case class Value[A](name: String) extends Parser[A, Token] {
    self =>

    case class Result[+A](a: A) extends Token
    def apply(a: A) = Result(a)

    def parse(in: Input[Token], cm: Boolean) = {
      if (in.nonEmpty) {
        in.head match {
          case self.Result(a: A) =>
            (a, in.tail)
          case _ =>
            fail("expected keyword: '" + name + "'", in, cm)
        }
      } else {
        fail("unexpected end of input", in, cm)
      }
    }
  }
}
