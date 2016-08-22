package arse.bi

import arse._

trait Mixfix[Op, Expr, S] extends Rel[Expr, S] {
  import Mixfix.Min
  import Mixfix.Max

  def inner_expr: Rel[Expr, S]
  def apply(op: Op, args: List[Expr]): Expr

  def prefix_op: Rel[(Op, Int), S]
  def postfix_op: Rel[(Op, Int), S]
  def infix_op: Rel[(Op, (Assoc, Int)), S]

  def unary(op: Op, arg: Expr) = apply(op, List(arg))
  def binary(op: Op, arg1: Expr, arg2: Expr) = apply(op, List(arg1, arg2))

  /*
  def parse_mixfix_arg(lower: Int, s: S) = {
    {
      prefix_app(lower, s)
    } or {
      inner_expr(s)
    }
  }

  def parse_mixfix_app(lower: Int, s0: S): (Expr, S) = {
    val (left, s1) = parse_mixfix_arg(lower, s0)
    parse_postinfix_app(lower, Max, left, s1)
  }

  def apply(s: S) = parse_mixfix_app(Min, s)
  */
}