// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

import scala.reflect.ClassTag
import scala.annotation.Annotation

package object arse {
  trait Backtrack extends Throwable

  object Fail extends Backtrack {
    override def toString = "<generic failure>"
    override def fillInStackTrace = this
    override val getStackTrace = Array[StackTraceElement]()
  }

  case class Abort[T](msg: String, in: List[T]) extends Exception {
    override def toString = {
      val (toks, rest) = in.splitAt(4)
      val at = toks.mkString(" ") + (if (rest == Nil) "" else "...")
      msg + " at '" + at + "'"
    }
  }

  def fail = throw Fail
  def abort[T](msg: String, in: List[T]) = throw Abort(msg, in)

  implicit class Control[A](first: => A) {
    def or[B <: A](second: => B) = {
      try {
        first
      } catch {
        case _: Backtrack =>
          second
      }
    }

    def mask[E <: Throwable](implicit ev: ClassTag[E]) = {
      try {
        first
      } catch {
        case _: E =>
          fail
      }
    }
  }
}
