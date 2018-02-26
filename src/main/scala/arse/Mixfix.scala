// ARSE Parser libary
// (c) 2017 Gidon Ernst <gidonernst@gmail.com>
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

  def prefix_op(op: Parser[Op]) = op map { n => (n, prefix_ops.getOrElse(n, backtrack())) }
  def postfix_op(op: Parser[Op]) = op map { n => (n, postfix_ops.getOrElse(n, backtrack())) }
  def infix_op(op: Parser[Op]) = op map { n => (n, infix_ops.getOrElse(n, backtrack())) }
}

case class Mixfix[Op, Expr](
  name: String,
  inner_expr: () => Parser[Expr],
  parse: (Op, List[Expr]) => Expr,
  prefix_op: Parser[(Op, Int)],
  postfix_op: Parser[(Op, Int)],
  infix_op: Parser[(Op, (Assoc, Int))],
  min: Int,
  max: Int)
  extends Parser[Expr] {

  def nprec(assoc: Assoc, prec: Int) = if (assoc == Left) prec else prec - 1
  def rprec(assoc: Assoc, prec: Int) = if (assoc == Right) prec else prec + 1

  def unary(op: Op, arg: Expr) = {
    parse(op, List(arg))
  }

  def binary(op: Op, arg1: Expr, arg2: Expr) = {
    parse(op, List(arg1, arg2))
  }

  def prefix_app(lower: Int, in: Input, cm: Boolean) = {
    val (op, prec) = prefix_op parse (in, false)
    if (prec < lower) backtrack()
    val right = mixfix_app(prec, in, true)
    unary(op, right)
  }

  def postfix_app(lower: Int, upper: Int, left: Expr, in: Input, cm: Boolean) = {
    val (op, prec) = postfix_op parse (in, false)
    if (prec < lower || upper < prec) backtrack()
    postinfix_app(lower, prec, unary(op, left), in, true)
  }

  def infix_app(lower: Int, upper: Int, left: Expr, in: Input, cm: Boolean) = {
    val (op, (assoc, prec)) = infix_op parse (in, false)
    if (prec < lower || upper < prec) backtrack()
    val right = mixfix_app(rprec(assoc, prec), in, true)
    postinfix_app(lower, nprec(assoc, prec), binary(op, left, right), in, cm)
  }

  def postinfix_app(lower: Int, upper: Int, left: Expr, in: Input, cm: Boolean): Expr = {
    val back = in.position

    {
      in.position = back
      infix_app(lower, upper, left, in, false)
    } or {
      in.position = back
      postfix_app(lower, upper, left, in, false)
    } or {
      in.position = back
      left
    }
  }

  def mixfix_arg(lower: Int, in: Input, cm: Boolean) = {
    val pos = in.position

    {
      prefix_app(lower, in, false)
    } or {
      inner_expr() parseAt (pos, in, cm)
    }
  }

  def mixfix_app(lower: Int, in: Input, cm: Boolean): Expr = {
    val left = mixfix_arg(lower, in, cm)
    postinfix_app(lower, max, left, in, true)
  }

  def parse(in: Input, cm: Boolean) = {
    mixfix_app(min, in, cm)
  }

  def above(lower: Int) = new Parser[Expr]() {
    def format = name
    def parse(in: Input, cm: Boolean) = mixfix_app(lower, in, cm)
  }

  override def toString = {
    name
  }
}
