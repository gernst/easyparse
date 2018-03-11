// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import java.util.regex.Pattern

import bk.Control
import implicits.ListParser
import implicits.Parser2

trait Parser[+A] {
  p =>

  def parse(in: Input, cm: Boolean): A

  def parse(in: Input): A = {
    parse(in, true)
  }

  def parseAt(pos: Int, in: Input, cm: Boolean): A = {
    in.position = pos
    parse(in, cm)
  }

  def $ = new End(this)
  def ~[B](s: String): Parser[A] = p <~ new Literal(s)
  def ~[B](q: Parser[B]): Parser[A ~ B] = new Sequence(p, q)

  def <~[B](q: Parser[B]): Parser[A] = (p ~ q)._1
  def ~>[B](q: Parser[B]): Parser[B] = (p ~ q)._2

  def |[B >: A](q: Parser[B]): Parser[B] = new Choice(p, q)
  def map[B](f: A => B): Parser[B] = new Attribute(p, f)

  def ?(): Parser[Option[A]] = new Repeat(p, 0, 1) map {
    case List() => None
    case List(a) => Some(a)
  }

  def *(): Parser[List[A]] = new Repeat(p, 0, Int.MaxValue)
  def +(): Parser[List[A]] = new Repeat(p, 1, Int.MaxValue)

  def ~*(sep: String): Parser[List[A]] = p :: (sep ~ p).* | ret(Nil)
  def ~*(sep: Parser[_]): Parser[List[A]] = p :: (sep ~> p).* | ret(Nil)

  def ~+(sep: String): Parser[List[A]] = p :: (sep ~ p).*
  def ~+(sep: Parser[_]): Parser[List[A]] = p :: (sep ~> p).*

  def filter(f: A => Boolean): Parser[A] = new Filter(p, f)
  def filterNot(f: A => Boolean): Parser[A] = new Filter(p, (a: A) => !f(a))
  def reduceLeft[B >: A](f: (B, A) => B): Parser[B] = p.+ map (_ reduceLeft f)
  def reduceRight[B >: A](f: (A, B) => B): Parser[B] = p.+ map (_ reduceRight f)
  def foldLeft[B](z: => Parser[B])(f: (B, A) => B): Parser[B] = (z ~ p.*) map { case b ~ as => as.foldLeft(b)(f) }
  def foldRight[B](z: => Parser[B])(f: (A, B) => B): Parser[B] = (p.* ~ z) map { case as ~ b => as.foldRight(b)(f) }
}

class Recursive[A](name: String, p: () => Parser[A]) extends Parser[A] {
  def parse(in: Input, cm: Boolean) = p() parse (in, cm)
  override def toString = name
}

case object Fail extends Parser[Nothing] {
  def parse(in: Input, cm: Boolean) = fail(null, in, cm)
  override def toString = "fail"
}

class Accept[+A](a: A) extends Parser[A] {
  def parse(in: Input, cm: Boolean) = a
  override def toString = "accept"
}

class End[+A](p: Parser[A]) extends Parser[A] {
  def parse(in: Input, cm: Boolean) = {
    val a = p parse (in, cm)
    if (!in.isEmpty) fail("end", in, cm)
    a
  }

  override def toString = {
    "end"
  }
}

class Literal(token: String) extends Parser[String] {
  def parse(in: Input, cm: Boolean) = {
    if (in.text.startsWith(token, in.position)) {
      in advanceBy token.length
      token
    } else {
      fail(token, in, cm)
    }
  }

  override def toString = {
    token
  }
}

class Regex(name: String, pattern: String) extends Parser[String] {
  val regex = Pattern.compile(pattern)
  val matcher = regex.matcher("")

  def matches(text: String, pos: Int) = {
    matcher.reset(text)
    matcher.region(pos, text.length)
    // print("match " + pattern + " at '" + text.substring(pos) + "' ")

    if (matcher.lookingAt()) {
      val end = matcher.end
      // println()
      // println("remaining input '" + text.substring(end) + "'")
      Some(end)
    } else {
      // println("failed")
      None
    }
  }

  def parse(in: Input, cm: Boolean) = {
    matcher.reset(in.text)
    matcher.region(in.position, in.text.length)

    matches(in.text, in.position) match {
      case Some(next) =>
        in advanceTo next
        matcher.group()
      case None =>
        fail(name, in, cm)
    }
  }

  override def toString = {
    name
  }
}

class Whitespace(pattern: String) extends Regex(" ", pattern) {
}

class Sequence[+A, +B](p: Parser[A], q: Parser[B]) extends Parser[A ~ B] {
  def parse(in: Input, cm: Boolean) = {
    val a = p parse (in, cm)
    val b = q parse (in, true)
    (a, b)
  }

  override def toString = {
    "(" + p + " ~ " + q + ")"
  }
}

class Choice[+A](p: Parser[A], q: Parser[A]) extends Parser[A] {
  def parse(in: Input, cm: Boolean) = {
    val pos = in.position

    {
      p parseAt (pos, in, false)
    } or {
      q parseAt (pos, in, cm)
    }
  }

  override def toString = {
    "(" + p + " | " + q + ")"
  }
}

class Repeat[+A](p: Parser[A], min: Int, max: Int) extends Parser[List[A]] {
  def parse(in: Input, cm: Boolean): List[A] = {
    parse(0, in, cm)
  }

  def parse(done: Int, in: Input, cm: Boolean): List[A] = {
    val pos = in.position

    if (done < min) {
      val a = p parse (in, cm)
      val as = this parse (done + 1, in, cm)
      a :: as
    } else if (done < max)
      {
        val a = p parse (in, false)
        val as = this parse (done + 1, in, cm)
        a :: as
      } or {
        in.position = pos
        Nil
      }
    else {
      Nil
    }
  }

  override def toString = {
    "(" + p + ") {" + min + "," + max + "}"
  }
}

class Attribute[A, +B](p: Parser[A], f: A => B) extends Parser[B] {
  def parse(in: Input, cm: Boolean) = {
    val a = p parse (in, cm)
    f(a)
  }

  override def toString = {
    p.toString
  }
}

case class Filter[A](p: Parser[A], f: A => Boolean) extends Parser[A] {
  def parse(in: Input, cm: Boolean) = {
    val a = p parse (in, cm)
    if (!f(a)) fail(p.toString, in, cm)
    else a
  }

  override def toString = {
    p.toString
  }
}
