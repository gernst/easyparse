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

trait Syntax[Op] {
  def prefix_ops: Map[Op, Int]
  def postfix_ops: Map[Op, Int]
  def infix_ops: Map[Op, (Assoc, Int)]

  def contains(op: Op) =
    (prefix_ops contains op) ||
      (postfix_ops contains op) ||
      (infix_ops contains op)

  def prefix_op(op: Parser[Op]) = op map prefix_ops
  def postfix_op(op: Parser[Op]) = op map postfix_ops
  def infix_op(op: Parser[Op]) = op map infix_ops
}

case class Mixfix[Op, Expr](name: String,
  inner_expr: () => Parser[Expr],
  apply: (Op, List[Expr]) => Expr,
  prefix_op: Parser[(Op, Int)],
  postfix_op: Parser[(Op, Int)],
  infix_op: Parser[(Op, (Assoc, Int))],
  min: Int,
  max: Int)
    extends Parser[Expr] {

  def nprec(assoc: Assoc, prec: Int) = if (assoc == Left) prec else prec - 1
  def rprec(assoc: Assoc, prec: Int) = if (assoc == Right) prec else prec + 1

  def unary(op: Op, arg: Expr) = {
    apply(op, List(arg))
  }

  def binary(op: Op, arg1: Expr, arg2: Expr) = {
    apply(op, List(arg1, arg2))
  }

  def prefix_app(lower: Int, in: Input) = {
    val (op, prec) = prefix_op(in)
    if (prec < lower) backtrack()
    val right = mixfix_app(prec, in)
    unary(op, right)
  }

  def postfix_app(lower: Int, upper: Int, left: Expr, in: Input) = {
    val (op, prec) = postfix_op(in)
    if (prec < lower || upper < prec) backtrack()
    postinfix_app(lower, prec, unary(op, left), in)
  }

  def infix_app(lower: Int, upper: Int, left: Expr, in: Input) = {
    val (op, (assoc, prec)) = infix_op(in)
    if (prec < lower || upper < prec) backtrack()
    val right = mixfix_app(rprec(assoc, prec), in)
    postinfix_app(lower, nprec(assoc, prec), binary(op, left, right), in)
  }

  def postinfix_app(lower: Int, upper: Int, left: Expr, in: Input): Expr = {
    val back = in.position

    {
      in.position = back
      infix_app(lower, upper, left, in)
    } or {
      in.position = back
      postfix_app(lower, upper, left, in)
    } or {
      in.position = back;
      left
    }
  }

  def mixfix_arg(lower: Int, in: Input) = {
    val back = in.position

    {
      in.position = back
      prefix_app(lower, in)
    } or {
      in.position = back
      inner_expr()(in)
    }
  }

  def mixfix_app(lower: Int, in: Input): Expr = {
    val left = mixfix_arg(lower, in)
    postinfix_app(lower, max, left, in)
  }

  def apply(in: Input) = {
    mixfix_app(min, in)
  }

  def above(lower: Int) = new Parser[Expr]() {
    def format = name
    def apply(in: Input) = mixfix_app(lower, in)
  }
}
