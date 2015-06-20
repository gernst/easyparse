package arse

import control._

trait Mixfix extends Combinators {
  import Combinators._

  type Op
  type Expr

  def Min = 0
  def Max = Int.MaxValue

  val inner_expr: Parser[T, Expr]

  def prefix_op: Parser[T, (Op, Int)]
  def postfix_op: Parser[T, (Op, Int)]
  def infix_op: Parser[T, (Op, (Assoc, Int))]

  def unary(op: Op, arg: Expr): Expr
  def binary(op: Op, arg1: Expr, arg2: Expr): Expr

  def prefix_app(lower: Int, in0: Input) = {
    val ((op, prec), in1) = prefix_op(in0)
    if(prec < lower) fail
    val (right, in2) = mixfix_app(prec, in1)
    (unary(op, right), in2)
  }

  def postfix_app(lower: Int, upper: Int, left: Expr, in0: Input) = {
    val ((op, prec), in1) = postfix_op(in0)
    if(prec < lower || upper < prec) fail
    postinfix_app(lower, prec, unary(op, left), in1)
  }

  private def nprec(assoc: Assoc, prec: Int) = if (assoc == Left) prec else prec - 1
  private def rprec(assoc: Assoc, prec: Int) = if (assoc == Right) prec else prec + 1

  def infix_app(lower: Int, upper: Int, left: Expr, in0: Input) = {
    val ((op, (assoc, prec)), in1) = infix_op(in0)
    if(prec < lower || upper < prec) fail
    val (right, in2) = mixfix_app(rprec(assoc, prec), in1)
    postinfix_app(lower, nprec(assoc, prec), binary(op, left, right), in2)
  }

  def postinfix_app(lower: Int, upper: Int, left: Expr, in: Input): (Expr, Input) = {
    {
      infix_app(lower, upper, left, in)
    } or {
      postfix_app(lower, upper, left, in)
    } or {
      (left, in)
    }
  }

  def mixfix_arg(lower: Int, in: Input) = {
    {
      prefix_app(lower, in)
    } or {
      inner_expr(in)
    }
  }

  def mixfix_app(lower: Int, in0: Input): (Expr, Input) = {
    val (left, in1) = mixfix_arg(lower, in0)
    postinfix_app(lower, Max, left, in1)
  }

  val mixfix_expr: Parser[T, Expr] = lift { mixfix_app(Min, _) }
}