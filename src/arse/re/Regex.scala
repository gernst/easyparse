package arse.re

sealed trait Regex {
  def first: Letters

  def isNullable: Boolean
  def derive(c: Letter): Regex
  def active(c: Letter): Groups
  def reset(c: Letter): Groups

  def |(that: Regex) = Regex.or(Set(this, that))
  def &(that: Regex) = Regex.and(Set(this, that))

  def unary_~() = this match {
    case Not(e) => e
    case Match(cs) => Match(Letters.alphabet -- cs)
    case _ => Not(this)
  }

  def ~(that: Regex) = (this, that) match {
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case (Epsilon, _) => that
    case (_, Epsilon) => this    
    case (Seq(first, second), third) => Seq(first, Seq(second, third))
    case _ => Seq(this, that)
  }

  def +(): Regex = {
    this ~ this.*
  }

  def *(): Regex = this match {
    case Empty | Epsilon => Epsilon
    case Rep(that) => that
    case _ => Rep(this)
  }
}

object Regex {
  def ors(e: Regex): Set[Regex] = e match {
    case Or(es) => es flatMap ors
    case _ => Set(e)
  }

  def ands(e: Regex): Set[Regex] = e match {
    case And(es) => es flatMap ands
    case _ => Set(e)
  }

  def or(es: Set[Regex]) = es flatMap ors match {
    case Set1(e) => e
    case rs => Or(rs)
  }

  def and(es: Set[Regex]) = es flatMap ands match {
    case Set1(e) => e
    case rs => And(rs)
  }

  def group(name: String, e: Regex) = e match {
    case Empty | Epsilon => e
    case _ => Group(name, e)
  }
}

case class Match(first: Letters) extends Regex {
  def isNullable = false
  def derive(c: Letter) = if (first contains c) Epsilon else Empty
  def active(c: Letter) = Set()
  def reset(c: Letter) = Set()
  override def toString = {
    if (first.size == 1 && Letter.isPrintable(first.head)) first.head.toChar.toString
    else Letters.compact(first)
  }
}

object Match {
  def apply(b: Int): Regex = {
    assert(0 <= b && b < 256)
    Match(Letters.empty + b)
  }

  def apply(c: Char): Regex = {
    // TODO: works on little endian
    val b0 = (c >> 0) & 0xFF
    val b1 = (c >> 8) & 0xFF
    if (b1 == 0) Match(b0)
    else Match(b1) ~ Match(b0)
  }
}

case class Group(name: String, e: Regex) extends Regex {
  def first = e.first
  def isNullable = e.isNullable
  def derive(c: Letter) = Regex.group(name, e derive c)
  def active(c: Letter) = if(first contains c) (e active c) + name else (e active c)
  def reset(c: Letter) = ??? // if(first contains c) (e active c) + name else (e active c)
  override def toString = "(" + name + ":" + e + ")"
}

case class Seq(e1: Regex, e2: Regex) extends Regex {
  def first = if (e1.isNullable) e1.first | e2.first else e1.first
  def isNullable = e1.isNullable && e2.isNullable

  def derive(c: Letter) = if (e1.isNullable) {
    ((e1 derive c) ~ e2) | (e2 derive c)
  } else {
    ((e1 derive c) ~ e2)
  }
  def active(c: Letter) = if (e1.isNullable) {
    (e1 active c) | (e2 active c)
  } else {
    (e1 active c)
  }
  def reset(c: Letter) = if (e1.isNullable) {
    (e1 reset c) | (e2 reset c)
  } else {
    (e1 reset c)
  }
  override def toString = e1 + "" + e2
}

case class Not(e: Regex) extends Regex {
  def first = Letters.alphabet -- e.first
  def isNullable = e.isNullable
  def derive(c: Letter) = ~(e derive c)
  def active(c: Letter) = e active c
  def reset(c: Letter) = e reset c
  override def toString = e match {
    case Empty => "."
    case _ => "~(" + e.toString + ")"
  }
}

case class Rep(e: Regex) extends Regex {
  def first = e.first
  def isNullable = true
  def derive(c: Letter) = (e derive c) ~ this
  def active(c: Letter) = e active c
  def reset(c: Letter) = e reset c
  override def toString = e match {
    case _: Seq => "(" + e + ")*"
    case _ => e + "*"
  }
}

case class Or(es: Set[Regex]) extends Regex {
  def first = es.foldLeft(Letters.empty)(_ | _.first)
  def isNullable = es exists (_.isNullable)
  def derive(c: Letter) = Regex.or(es map (_ derive c))
  def active(c: Letter) = es.flatMap(_ active c)
  def reset(c: Letter) = es.flatMap(_ reset c)
  override def toString = {
    if (es.isEmpty) "{}" else es mkString ("(", " | ", ")")
  }
}

case class And(es: Set[Regex]) extends Regex {
  def first = es.foldLeft(Letters.empty)(_ & _.first)
  def isNullable = es forall (_.isNullable)
  def derive(c: Letter) = if (es.isEmpty) Empty else Regex.and(es map (_ derive c))
  def active(c: Letter) = es.flatMap(_ active c)
  def reset(c: Letter) = es.flatMap(_ reset c)
  override def toString = {
    if (es.isEmpty) "$" else es mkString ("(", " & ", ")")
  }
}
