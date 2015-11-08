// ARSE Parser libary
// (c) 2015 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

sealed trait Assoc
case object Non extends Assoc {
  override def toString = "non"
}

case object Left extends Assoc {
  override def toString = "left"
}

case object Right extends Assoc {
  override def toString = "right"
}

trait Fixity

case object Nilfix extends Fixity {
  override def toString = "nilfix"
}

case class Prefix(prec: Int) extends Fixity {
  override def toString = "prefix " + prec
}

case class Postfix(prec: Int) extends Fixity {
  override def toString = "postfix " + prec
}

case class Infix(assoc: Assoc, prec: Int) extends Fixity {
  override def toString = "infix " + assoc + " " + prec
}
