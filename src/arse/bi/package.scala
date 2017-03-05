package arse

import scala.language.implicitConversions

import arse.ll._

package object bi {
  type ClassTag[A] = scala.reflect.ClassTag[A]

  def __[A](implicit ev: ClassTag[A]) = Tok[A]()
  def string = Tok[String]("String")

  def rec[A, S](p: => Rel[A, S]) = new Rec(p)

  def $[S] = Eof[S]()
  implicit def toElem[S](s: S) = Elem(s)
}