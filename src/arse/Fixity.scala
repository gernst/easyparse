// ARSE Parser libary
// (c) 2015 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

sealed trait Assoc
case object Non extends Assoc
case object Left extends Assoc
case object Right extends Assoc

trait Fixity
case object Nilfix extends Fixity
case class Prefix(prec: Int) extends Fixity
case class Postfix(prec: Int) extends Fixity
case class Infix(assoc: Assoc, prec: Int) extends Fixity
