package arse

import control._

trait Mixfix extends Combinators {
  import Combinators._

  type Op
  type Expr

  def Min = 0
  def Max = Int.MaxValue

  def inner_op: Parser[T, Op]
  def inner_arg: Parser[T, Expr]

  def prefix_op(lower: Int): Parser[T, (Op, Int)]
  def postfix_op(lower: Int, upper: Int): Parser[T, (Op, Int)]
  def infix_op(lower: Int, upper: Int): Parser[T, (Op, Int, Assoc)]

  def apply(op: Op, args: Seq[Expr]): Expr
  def unary(op: Op, arg: Expr) = apply(op, List(arg))
  def binary(op: Op, arg1: Expr, arg2: Expr) = apply(op, List(arg1, arg2))

  val normal_app = parse(apply _)(inner_op, inner_arg *)

  def prefix_app(lower: Int) = {
    prefix_op(lower) >> {
      case (op, prec) =>
        mixfix_app(prec) ^^ { unary(op, _) }
    }
  }

  def postfix_app(lower: Int, upper: Int, left: Expr): Parser[T, Expr] = {
    postfix_op(lower, upper) >> {
      case (op, prec) =>
        postinfix_app(lower, prec, unary(op, left))
    }
  }

  def nprec(assoc: Assoc, prec: Int) = if (assoc == Left) prec else prec - 1
  def rprec(assoc: Assoc, prec: Int) = if (assoc == Right) prec else prec + 1

  def infix_app(lower: Int, upper: Int, left: Expr): Parser[T, Expr] = {
    infix_op(lower, upper) >> {
      case (op, prec, assoc) =>
        mixfix_app(rprec(assoc, prec)) >> {
          right =>
            postinfix_app(lower, nprec(assoc, prec), binary(op, left, right))
        }
    }
  }

  def postinfix_app(lower: Int, upper: Int, left: Expr) = {
    infix_app(lower, upper, left) | postfix_app(lower, upper, left) | ret(left)
  }

  def mixfix_arg(lower: Int) = {
    prefix_app(lower) | normal_app
  }

  def mixfix_app(lower: Int): Parser[T, Expr] = {
    mixfix_arg(lower) >> { postinfix_app(lower, Max, _) }
  }
}