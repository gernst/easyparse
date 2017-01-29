package arse

import scala.collection.immutable.BitSet

package object re {
  type Letter = Int
  type Letters = BitSet
  type State = Regex
  type States = Set[State]
  type Groups = Set[String]
  type Transitions = Map[(State, Letter), (Groups, Groups, State)]

  object Set1 {
    def unapply[A](s: Set[A]) = {
      if (s.size == 1) Some(s.head)
      else None
    }
  }

  val Epsilon: Regex = And(Set())
  val Empty: Regex = Or(Set())
}