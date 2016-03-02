// ARSE Parser libary
// (c) 2015 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

import control._

trait MixfixPlus extends Combinators {
  import Combinators._

  type Op
  type Expr

  def Min = 0
  def Max = Int.MaxValue

  val inner_expr: Parser[T, Expr]

  def outfix_op: Parser[T, (Op, List[String])]
  def prefix_op: Parser[T, (Op, (Int, List[String]))]
  def postfix_op: Parser[T, (Op, (Int, List[String]))]
  def infix_op: Parser[T, (Op, (Assoc, Int, List[String]))]

  def nary(op: Op, args: List[Expr]): Expr

  def mixfix_args(seps: List[String], rprec: Option[Int], in0: Input): (List[Expr], Input) = seps match {
    case Nil =>
      rprec match {
        case None =>
          (Nil, in0)
        case Some(rprec) =>
          val (right, in1) = mixfix_app(rprec, in0)
          (List(right), in1)
      }

    case sep :: rest =>
      val (arg, in1) = mixfix_expr(in0)
      val (args, in2) = mixfix_args(rest, rprec, in1)
      (arg :: args, in2)
  }

  def prefix_app(lower: Int, in0: Input) = {
    val ((op, (prec, seps)), in1) = prefix_op(in0)
    if (prec < lower) fail
    val (args, in2) = mixfix_args(seps, Some(prec), in1)
    (nary(op, args), in2)
  }

  def outfix_app(in0: Input) = {
    val ((op, seps), in1) = outfix_op(in0)
    val (args, in2) = mixfix_args(seps, None, in1)
    (nary(op, args), in2)
  }

  def postfix_app(lower: Int, upper: Int, left: Expr, in0: Input) = {
    val ((op, (prec, seps)), in1) = postfix_op(in0)
    if (prec < lower || upper < prec) fail
    val (args, in2) = mixfix_args(seps, None, in1)
    postinfix_app(lower, prec, nary(op, left :: args), in2)
  }

  private def nprec(assoc: Assoc, prec: Int) = if (assoc == Left) prec else prec - 1
  private def rprec(assoc: Assoc, prec: Int) = if (assoc == Right) prec else prec + 1

  def infix_app(lower: Int, upper: Int, left: Expr, in0: Input) = {
    val ((op, (assoc, prec, seps)), in1) = infix_op(in0)
    if (prec < lower || upper < prec) fail
    val (args, in2) = mixfix_args(seps, Some(rprec(assoc, prec)), in1)
    postinfix_app(lower, nprec(assoc, prec), nary(op, left :: args), in2)
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

  def preoutfix_app(lower: Int, in: Input) = {
    {
      prefix_app(lower, in)
    } or {
      outfix_app(in)
    } or {
      inner_expr(in)
    }
  }

  def mixfix_app(lower: Int, in0: Input): (Expr, Input) = {
    val (left, in1) = preoutfix_app(lower, in0)
    postinfix_app(lower, Max, left, in1)
  }

  val mixfix_expr: Parser[T, Expr] = lift { mixfix_app(Min, _) }
}
