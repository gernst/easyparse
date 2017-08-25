// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import arse.control._

sealed trait Assoc
case object Non extends Assoc
case object Left extends Assoc
case object Right extends Assoc

trait Fixity
case object Nilfix extends Fixity
case class Prefix(prec: Int) extends Fixity
case class Postfix(prec: Int) extends Fixity
case class Infix(assoc: Assoc, prec: Int) extends Fixity

trait Syntax[T] {
  def prefix_ops: Map[T, Int]
  def postfix_ops: Map[T, Int]
  def infix_ops: Map[T, (Assoc, Int)]

  def contains(t: T) =
    (prefix_ops contains t) ||
      (postfix_ops contains t) ||
      (infix_ops contains t)

  def prefix_op[O](op: T => O) = mixfix_op((t: T) => prefix_ops.get(t), op)
  def postfix_op[O](op: T => O) = mixfix_op((t: T) => postfix_ops.get(t), op)
  def infix_op[O](op: T => O) = mixfix_op((t: T) => infix_ops.get(t), op)

  def mixfix_op[T, O, A](m: T => Option[A], op: T => O) = new Parser[List[T], (O, A)]() {
    def apply(s: List[T]) = s match {
      case t :: s => m(t) match {
        case Some(a) => ((op(t), a), s)
        case None => fail
      }
      case _ => fail
    }
    def format = {
      "op"
    }
  }

  def mixfix_op_test[T, O, A](m: T => Boolean, op: T => O) = new Parser[List[T], O]() {
    def apply(s: List[T]) = s match {
      case t :: s => m(t) match {
        case true => (op(t), s)
        case false => fail
      }
      case _ => fail
    }
    def format = {
      "op"
    }
  }
}

case class Mixfix[S, O, E](name: String,
                           inner_expr: () => Parser[S, E],
                           apply: (O, List[E]) => E,
                           prefix_op: Parser[S, (O, Int)],
                           postfix_op: Parser[S, (O, Int)],
                           infix_op: Parser[S, (O, (Assoc, Int))],
                           min: Int,
                           max: Int)
    extends Parser[S, E] {

  def nprec(assoc: Assoc, prec: Int) = if (assoc == Left) prec else prec - 1
  def rprec(assoc: Assoc, prec: Int) = if (assoc == Right) prec else prec + 1

  def format = {
    name
  }

  def unary(op: O, arg: E) = {
    apply(op, List(arg))
  }

  def binary(op: O, arg1: E, arg2: E) = {
    apply(op, List(arg1, arg2))
  }

  def prefix_app(lower: Int, s0: S) = {
    val ((op, prec), s1) = prefix_op(s0)
    if (prec < lower) fail
    val (right, s2) = mixfix_app(prec, s1)
    (unary(op, right), s2)
  }

  def postfix_app(lower: Int, upper: Int, left: E, s0: S) = {
    val ((op, prec), s1) = postfix_op(s0)
    if (prec < lower || upper < prec) fail
    postinfix_app(lower, prec, unary(op, left), s1)
  }

  def infix_app(lower: Int, upper: Int, left: E, s0: S) = {
    val ((op, (assoc, prec)), s1) = infix_op(s0)
    if (prec < lower || upper < prec) fail
    val (right, s2) = mixfix_app(rprec(assoc, prec), s1)
    postinfix_app(lower, nprec(assoc, prec), binary(op, left, right), s2)
  }

  def postinfix_app(lower: Int, upper: Int, left: E, s: S): (E, S) = {
    {
      infix_app(lower, upper, left, s)
    } or {
      postfix_app(lower, upper, left, s)
    } or {
      (left, s)
    }
  }

  def mixfix_arg(lower: Int, s: S) = {
    {
      prefix_app(lower, s)
    } or {
      inner_expr()(s)
    }
  }

  def mixfix_app(lower: Int, s0: S): (E, S) = {
    val (left, s1) = mixfix_arg(lower, s0)
    postinfix_app(lower, max, left, s1)
  }

  def apply(s: S) = {
    mixfix_app(min, s)
  }
  
  def above(lower: Int) = new Parser[S, E]() {
    def format = name
    def apply(s: S) = mixfix_app(lower, s)
  }
}