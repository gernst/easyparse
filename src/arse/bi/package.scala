package arse

import scala.language.implicitConversions

import arse._

package object bi {
  type ClassTag[A] = scala.reflect.ClassTag[A]

  def __[A] = Tok[A]()
  def string = Tok[String]("String")

  def rec[A, S](p: => Rel[A, S]) = new Rec(p)

  implicit def toLit[A](a: A) = new Lit(a)
}