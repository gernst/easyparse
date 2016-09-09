package arse.bi

import arse._

trait Mixfix[Op, Expr, S] extends Rel[Expr, S] {
  import Mixfix.Min
  import Mixfix.Max
  import Mixfix.nprec
  import Mixfix.rprec

  def inner_expr: Rel[Expr, S]

  def apply: ((Op, List[Expr]) <=> Expr)
  def make = apply

  object unary extends ((Op, Expr) <=> Expr) {
    def apply(a: (Op, Expr)): Expr = {
      val (op, arg) = a
      make(op, List(arg))
    }

    def inverse[C >: Expr](c: C) = {
      (~make)(c) match {
        case (op, List(arg)) => (op, arg)
        case _ => fail
      }
    }
  }
  
  object binary extends ((Op, Expr, Expr) <=> Expr) {
    def apply(a: (Op, Expr, Expr)): Expr = {
      val (op, arg1, arg2) = a
      make(op, List(arg1, arg2))
    }

    def inverse[C >: Expr](c: C) = {
      (~make)(c) match {
        case (op, List(arg1, arg2)) => (op, arg1, arg2)
        case _ => fail
      }
    }
  }

  def prefix_op: Rel[(Op, Int), S]
  def postfix_op: Rel[(Op, Int), S]
  def infix_op: Rel[(Op, (Assoc, Int)), S]

  def parse(s: S) = {
    _parse.mixfix_app(Min, s)
  }

  def format[B >: Expr](bs: (B, S)) = {
    _format.mixfix_app(Max, bs)
  }

  object _parse {
    def prefix_app(lower: Int, s0: S) = {
      val ((op, prec), s1) = prefix_op parse s0
      if (prec < lower) fail
      val (right, in2) = mixfix_app(prec, s1)
      (unary(op, right), in2)
    }

    def postfix_app(lower: Int, upper: Int, left: Expr, s0: S) = {
      val ((op, prec), s1) = postfix_op parse s0
      if (prec < lower || upper < prec) fail
      postinfix_app(lower, prec, unary(op, left), s1)
    }

    def infix_app(lower: Int, upper: Int, left: Expr, s0: S) = {
      val ((op, (assoc, prec)), s1) = infix_op parse s0
      if (prec < lower || upper < prec) fail
      val (right, in2) = mixfix_app(rprec(assoc, prec), s1)
      postinfix_app(lower, nprec(assoc, prec), binary(op, left, right), in2)
    }

    def postinfix_app(lower: Int, upper: Int, left: Expr, in: S): (Expr, S) = {
      {
        infix_app(lower, upper, left, in)
      } or {
        postfix_app(lower, upper, left, in)
      } or {
        (left, in)
      }
    }

    def mixfix_arg(lower: Int, in: S) = {
      {
        prefix_app(lower, in)
      } or {
        inner_expr parse in
      }
    }

    def mixfix_app(lower: Int, s0: S): (Expr, S) = {
      val (left, s1) = mixfix_arg(lower, s0)
      postinfix_app(lower, Max, left, s1)
    }
  }

  object _format {
    def prefix_app[B >: Expr](upper: Int, bs: (B, S)) = {
      val (b, s0) = bs
      val (op, arg) = (~unary)(b)
      val s1 = mixfix_app(upper, (arg, s0))
      val s2 = prefix_op format ((op, upper), s1)
      s2
    }

    def mixfix_app[B >: Expr](upper: Int, bs: (B, S)): S = {
      ???
    }
  }
}