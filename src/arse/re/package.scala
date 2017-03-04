package arse

import scala.collection.immutable.BitSet

package object re {

  trait Derivable {
    def first: Letters
    def isNullable: Boolean
    def derive(c: Letter): Derivable
  }

  trait ScannerLike {
    def ~(cs: Iterable[Byte]): (State, Int)
  }

  type Letter = Int
  type Letters = BitSet
  type State = Derivable
  type States = Set[State]
  type Groups = Set[String]
  type Transitions = Map[(State, Letter), State]

  object Set1 {
    def unapply[A](s: Set[A]) = {
      if (s.size == 1) Some(s.head)
      else None
    }
  }

  val Epsilon: Regex = And(Set())
  val Empty: Regex = Or(Set())
}