package arse

import scala.reflect.ClassTag

object control {
  trait Backtrack extends Throwable

  case object Fail extends Backtrack {
    override def toString = "<generic failure>"
    override def fillInStackTrace = this
    override val getStackTrace = Array[StackTraceElement]()
  }

  case class Error(override val toString: String) extends Exception
  case class Fatal(override val toString: String) extends Exception

  def none: Option[Nothing] = None
  def some[A](a: A): Option[A] = Some(a)
  
  def fail = throw Fail
  def error(msg: String) = throw Error(msg)
  def fatal(msg: String) = throw Fatal(msg)

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