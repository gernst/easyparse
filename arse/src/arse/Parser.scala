// ARSE Parser libary
// (c) 2020 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import java.util.regex.Pattern

import implicits.ListParser
import implicits.Parser2

trait Parser[+A, T] {
  p =>

  def parse(in: Input[T], cm: Boolean): Result[A, T]

  def parse(in: Input[T]): Result[A, T] = {
    parse(in, true)
  }

  def parseAll(in0: Input[T]): A = {
    val (a, in1) = p.parse(in0)
    if (in1 != Nil) fail("expected end of input", in1, true)
    else a
  }

  def ~[B](t: T): Parser[A, T] = p <~ new Literal(t)
  def ~[B](q: Parser[B, T]): Parser[A ~ B, T] =
    new Sequence(p, q, strict = true)
  def ?~[B](t: T): Parser[A, T] = p ?<~ new Literal(t)
  def ?~[B](q: Parser[B, T]): Parser[A ~ B, T] =
    new Sequence(p, q, strict = false)

  def <~[B](q: Parser[B, T]): Parser[A, T] = (p ~ q)._1
  def ~>[B](q: Parser[B, T]): Parser[B, T] = (p ~ q)._2
  def ?<~[B](q: Parser[B, T]): Parser[A, T] = (p ?~ q)._1
  def ?~>[B](q: Parser[B, T]): Parser[B, T] = (p ?~ q)._2

  def |[B >: A](q: Parser[B, T]): Parser[B, T] = new Choice(p, q)
  def map[B](f: A => B): Parser[B, T] = new Attribute(p, f, partial = false)
  def collect[B](f: A => B): Parser[B, T] = new Attribute(p, f, partial = true)

  def ?(): Parser[Option[A], T] = new Repeat(p, 0, 1) map {
    case List()  => None
    case List(a) => Some(a)
  }

  def *(): Parser[List[A], T] = new Repeat(p, 0, Int.MaxValue)
  def +(): Parser[List[A], T] = new Repeat(p, 1, Int.MaxValue)

  def ~*(sep: T): Parser[List[A], T] = p :: (sep ~ p).* | ret(Nil)
  def ~*(sep: Parser[_, T]): Parser[List[A], T] = p :: (sep ~> p).* | ret(Nil)

  def ~+(sep: T): Parser[List[A], T] = p :: (sep ~ p).*
  def ~+(sep: Parser[_, T]): Parser[List[A], T] = p :: (sep ~> p).*

  def filter(f: A => Boolean): Parser[A, T] = new Filter(p, f)
  def filterNot(f: A => Boolean): Parser[A, T] = new Filter(p, (a: A) => !f(a))
  def reduceLeft[B >: A](f: (B, A) => B): Parser[B, T] =
    p.+ map (_ reduceLeft f)
  def reduceRight[B >: A](f: (A, B) => B): Parser[B, T] =
    p.+ map (_ reduceRight f)
  def foldLeft[B](z: => Parser[B, T])(f: (B, A) => B): Parser[B, T] =
    (z ~ p.*) map { case b ~ as => as.foldLeft(b)(f) }
  def foldRight[B](z: => Parser[B, T])(f: (A, B) => B): Parser[B, T] =
    (p.* ~ z) map { case as ~ b => as.foldRight(b)(f) }
}

class Recursive[A, T](name: String, p: () => Parser[A, T])
    extends Parser[A, T] {
  def parse(in: Input[T], cm: Boolean) = p() parse (in, cm)
  override def toString = name
}

case object Fail extends Parser[Nothing, Any] {
  def parse(in: Input[Any], cm: Boolean) = fail("unexpected Input[T]", in, cm)
  override def toString = "fail"
}

class Accept[+A, T](a: A) extends Parser[A, T] {
  def parse(in: Input[T], cm: Boolean) = (a, in)
  override def toString = "accept"
}

class Literal[T](token: T) extends Parser[T, T] {
  def parse(in0: Input[T], cm: Boolean) = {
    if (!in0.isEmpty && token == in0.head) {
      (token, in0.tail)
    } else {
      fail("expected " + this, in0, cm)
    }
  }

  override def toString = {
    token.toString
  }
}

class Literals[T](tokens: T*) extends Parser[T, T] {
  def parse(in0: Input[T], cm: Boolean) = {
    if (!in0.isEmpty && (tokens contains in0.head)) {
      (in0.head, in0.tail)
    } else {
      fail("expected " + this, in0, cm)
    }
  }

  override def toString = {
    tokens.mkString(" | ")
  }
}

class Sequence[+A, +B, T](p: Parser[A, T], q: Parser[B, T], strict: Boolean)
    extends Parser[A ~ B, T] {
  def parse(in0: Input[T], cm: Boolean) = {
    val (a, in1) = p parse (in0, cm)
    val (b, in2) = q parse (in1, strict)
    ((a, b), in2)
  }

  override def toString = {
    if (strict) "(" + p + " ~ " + q + ")"
    else "(" + p + " ~? " + q + ")"
  }
}

class Choice[+A, T](p: Parser[A, T], q: Parser[A, T]) extends Parser[A, T] {
  def parse(in: Input[T], cm: Boolean) = {
    {
      p parse (in, false)
    } or {
      q parse (in, cm)
    }
  }

  override def toString = {
    "(" + p + " | " + q + ")"
  }
}

class Repeat[+A, T](p: Parser[A, T], min: Int, max: Int)
    extends Parser[List[A], T] {
  def parse(in: Input[T], cm: Boolean): Result[List[A], T] = {
    parse(0, in, cm)
  }

  def parse(done: Int, in0: Input[T], cm: Boolean): Result[List[A], T] = {
    if (done < min) {
      val (a, in1) = p parse (in0, cm)
      val (as, in2) = this parse (done + 1, in1, cm)
      (a :: as, in2)
    } else if (done < max) {
      val (a, in1) = p parse (in0, false)
      val (as, in2) = this parse (done + 1, in1, cm)
      (a :: as, in2)
    } or {
      (Nil, in0)
    }
    else {
      (Nil, in0)
    }
  }

  override def toString = {
    if (min == 0 && max == 1) "(" + p + ") ?"
    else if (min == 0 && max == Int.MaxValue) "(" + p + ") *"
    else if (min == 1 && max == Int.MaxValue) "(" + p + ") +"
    else "(" + p + ") {" + min + "," + max + "}"
  }
}

class Attribute[A, +B, T](p: Parser[A, T], f: A => B, partial: Boolean)
    extends Parser[B, T] {
  def parse(in0: Input[T], cm: Boolean) = f match {
    case f: PartialFunction[A, B] if partial =>
      val (a, in1) = p parse (in0, cm)
      val b = f.applyOrElse(
        a,
        fail("expected " + p + " (partial attribute)", in0, cm)
      )
      (b, in1)
    case _ =>
      val (a, in1) = p parse (in0, cm)
      val b = f(a)
      (b, in1)
  }

  override def toString = {
    p.toString
  }
}

case class Filter[A, T](p: Parser[A, T], f: A => Boolean) extends Parser[A, T] {
  def parse(in0: Input[T], cm: Boolean) = {
    val (a, in1) = p parse (in0, cm)
    if (!f(a)) fail("expected " + p + " (test failed)", in0, cm)
    else (a, in1)
  }

  override def toString = {
    p.toString
  }
}
