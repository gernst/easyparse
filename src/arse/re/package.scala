package arse

import scala.collection.immutable.BitSet

package object re {

  trait RegexLike {
    def first: Letters
    def isNullable: Boolean
    def derive(c: Letter): RegexLike
  }

  type Letter = Int
  type Letters = BitSet
  type State = RegexLike
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