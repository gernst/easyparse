// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import scala.reflect.ClassTag

object control {
  trait NoStackTrace {
    this: Throwable =>
    override def fillInStackTrace = this
    override val getStackTrace = Array[StackTraceElement]()
  }

  def backtrack() = {
    throw Backtrack
  }

  case class Error(msg: String, in: Input) extends Exception {
    override def toString = "expected " + msg + " at '" + (in.rest take 10) + "...'"
  }

  case object Backtrack extends Throwable with NoStackTrace {
    override def toString = "backtrack"
  }

  implicit class Control[A](first: => A) {
    def or[B <: A](second: => B) = {
      try {
        first
      } catch {
        case Backtrack =>
          second
      }
    }
  }
}