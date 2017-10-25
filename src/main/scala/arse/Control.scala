// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import scala.reflect.ClassTag

object control {
  trait NoStackTrace {
    this: Throwable =>
    override def fillInStackTrace = this
    override val getStackTrace = Array[StackTraceElement]()
  }

  case object Backtrack extends Throwable with NoStackTrace {
    override def toString = "backtrack"
  }

  def backtrack() = throw Backtrack

  implicit class Control[A](first: => A) {
    def or[B <: A](second: => B) = {
      try {
        first
      } catch {
        case Backtrack =>
          second
      }
    }
    
    def rollback(second: => Unit) = {
      try {
        first
      } catch {
      case e: Throwable =>
        second
        throw e
      }
    }

    def mask[E <: Throwable](implicit ev: ClassTag[E]) = {
      try {
        first
      } catch {
        case _: E =>
          backtrack()
      }
    }
  }
}