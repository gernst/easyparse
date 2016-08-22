package arse

import arse._

package object simple {
  // trait ~>[S, +A] extends (S => (A, S))

  def uncurry[A1, A2, B](f: A1 => A2 => B): (((A1, A2)) => B) = {
    case (a1, a2) => f(a1)(a2)
  }

  def tok[A](a: A): (List[A] ~> A) = new (List[A] ~> A) {
    def apply(s: List[A]) = s match {
      case `a` :: as => (a, as)
      case _ => fail
    }
  }

  def ret[A, S](a: A) = new (S ~> A) {
    def apply(s: S) = (a, s)
  }

  implicit class ListParser[S, A](p: => S ~> List[A]) {
    def ::(q: => S ~> A) = (q, p) map ((_ :: _))
    def ++(q: => S ~> List[A]) = (p, q) map (_ ++ _)
  }

  implicit class ParserList[T, A](p: => List[T] ~> A) {
    type S = List[T]

    def $(): S => A = { s: S =>
      p(s) match {
        case (a, Nil) => a
        case _ => fail
      }
    }
  }

  // implicit class Parser1[S, A](p: => S ~> A) {
  trait ~>[S, +A] extends (S => (A, S)) {
    def p = this

    def bind[B](f: A => (S ~> B)) = uncurry(f) compose p

    def ?(): S ~> Option[A] = (p map Some.apply) | ret(None)
    def *(): S ~> List[A] = p.foldRight(ret(Nil: List[A]))(_ :: _)
    def +(): S ~> List[A] = p :: (p *)

    def |[B >: A](r: => S ~> B): (S ~> B) = {
      val q = p; new (S ~> B) {
        def apply(s: S) = (q: S ~> B)(s) or r(s)
      }
    }

    def map[B](f: A => B) = {
      val q = p; new (S ~> B) {
        def apply(s0: S) = {
          val (a, s1) = q(s0)
          (f(a), s1)
        }
      }
    }

    def foldRight[B](z: => S ~> B)(f: (A, B) => B): (S ~> B) = {
      val q = p; new (S ~> B) {
        def apply(s0: S) = {
          val (a, s1) = q(s0)
          val (b, s2) = p(s1)
          (f(a, b), s2)
        } or {
          z(s0)
        }
      }
    }

    def foldLeft[B](z: => S ~> B)(f: (B, A) => B): (S ~> B) = {
      val q = p; new (S ~> B) {
        def apply(s0: S) = {
          val (a, s1) = p(s0)
          val (b, s2) = q(s1)
          (f(a, b), s2)
        } or {
          z(s0)
        }
      }
    }
  }

  implicit class Parser2[S, A1, A2](ps: => (S ~> A1, S ~> A2)) extends (S ~> (A1, A2)) {
    lazy val (p1, p2) = ps

    def apply(s: S): ((A1, A2), S) = map((_, _))(s)

    def map[B](f: (A1, A2) => B) = new (S ~> B) {
      def apply(s0: S) = {
        val (a1, s1) = p1(s0)
        val (a2, s2) = p2(s1)
        (f(a1, a2), s2)
      }
    }
  }

  implicit class Parser3[S, A1, A2, A3](ps: => (S ~> A1, S ~> A2, S ~> A3)) extends (S ~> (A1, A2, A3)) {
    lazy val (p1, p2, p3) = ps

    def apply(s: S) = map((_, _, _))(s)

    def map[B](f: (A1, A2, A3) => B) = new (S ~> B) {
      def apply(s0: S) = {
        val (a1, s1) = p1(s0)
        val (a2, s2) = p2(s1)
        val (a3, s3) = p3(s2)
        (f(a1, a2, a3), s3)
      }
    }
  }

  implicit class Parser4[S, A1, A2, A3, A4](ps: => (S ~> A1, S ~> A2, S ~> A3, S ~> A4)) extends (S ~> (A1, A2, A3, A4)) {
    lazy val (p1, p2, p3, p4) = ps

    def apply(s: S) = map((_, _, _, _))(s)

    def map[B](f: (A1, A2, A3, A4) => B) = new (S ~> B) {
      def apply(s0: S) = {
        val (a1, s1) = p1(s0)
        val (a2, s2) = p2(s1)
        val (a3, s3) = p3(s2)
        val (a4, s4) = p4(s3)
        (f(a1, a2, a3, a4), s4)
      }
    }
  }
}