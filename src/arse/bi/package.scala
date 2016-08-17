package arse

package object bi {
  type ClassTag[A] = scala.reflect.ClassTag[A]

  def __[A] = Tok[A]()
  def string = Tok[String]("String")

  def rec[A, S](p: => Rel[A, S]) = new Rec(p)

  implicit def toLit[A](a: A) = new Lit(a)
  implicit def toRel1[A1, B](f: A1 <=> B) = new RelFunction1(f)
  implicit def toRel2[A1, A2, B](f: (A1, A2) <=> B) = new RelFunction2(f)
}