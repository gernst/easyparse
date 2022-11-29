// ARSE Parser libary
// (c) 2022 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package easyparse

sealed trait Assoc
case object Non extends Assoc
case object Left extends Assoc
case object Right extends Assoc

trait Fixity { def prec: Int }
case object Nilfix extends Fixity { def prec = 0 }
case class Prefix(prec: Int) extends Fixity
case class Postfix(prec: Int) extends Fixity
case class Infix(assoc: Assoc, prec: Int) extends Fixity

trait Syntax[Op, T] {
  def prefix_ops: Map[Op, Int]
  def postfix_ops: Map[Op, Int]
  def infix_ops: Map[Op, (Assoc, Int)]

  def ops = (prefix_ops.keys ++ postfix_ops.keys ++ infix_ops.keys).toSeq

  def contains(op: Op) =
    (prefix_ops contains op) ||
      (postfix_ops contains op) ||
      (infix_ops contains op)

  def prec(op: Op) = {
    if (prefix_ops contains op) prefix_ops(op)
    else if (postfix_ops contains op) postfix_ops(op)
    else if (infix_ops contains op) infix_ops(op)._2
    else throw new NoSuchElementException
  }

  def prefix_op(op: Parser[Op, T]) = op map { n =>
    (n, prefix_ops.getOrElse(n, backtrack("prefix operator expected")))
  }
  def postfix_op(op: Parser[Op, T]) = op map { n =>
    (n, postfix_ops.getOrElse(n, backtrack("postfix operator expected")))
  }
  def infix_op(op: Parser[Op, T]) = op map { n =>
    (n, infix_ops.getOrElse(n, backtrack("infix operator expected")))
  }
}

case class Mixfix[Op, Expr, T](
    name: String,
    inner_expr: () => Parser[Expr, T],
    apply: (Op, List[Expr]) => Expr,
    prefix_op: Parser[(Op, Int), T],
    postfix_op: Parser[(Op, Int), T],
    infix_op: Parser[(Op, (Assoc, Int)), T],
    min: Int,
    max: Int
) extends Parser[Expr, T] {

  def nprec(assoc: Assoc, prec: Int) = if (assoc == Left) prec else prec - 1
  def rprec(assoc: Assoc, prec: Int) = if (assoc == Right) prec else prec + 1

  def unary(op: Op, arg: Expr) = {
    apply(op, List(arg))
  }

  def binary(op: Op, arg1: Expr, arg2: Expr) = {
    apply(op, List(arg1, arg2))
  }

  def prefix_app(lower: Int, in0: Input[T], cm: Boolean) = {
    val ((op, prec), in1) = prefix_op parse (in0, false)
    if (prec < lower) backtrack("prefix operator of precedence >= " + lower + " expected")
    val (right, in2) = mixfix_app(prec, in1, true)
    val result = unary(op, right)
    (result, in2)
  }

  def postfix_app(
      lower: Int,
      upper: Int,
      left: Expr,
      in0: Input[T],
      cm: Boolean
  ) = {
    val ((op, prec), in1) = postfix_op parse (in0, false)
    if (prec < lower || upper < prec) backtrack("postfix operator of precedence >= " + lower + " and <= " + upper + " expected")
    postinfix_app(lower, prec, unary(op, left), in1, true)
  }

  def infix_app(
      lower: Int,
      upper: Int,
      left: Expr,
      in0: Input[T],
      cm: Boolean
  ) = {
    val ((op, (assoc, prec)), in1) = infix_op parse (in0, false)
    if (prec < lower || upper < prec) backtrack("infix operator of precedence >= " + lower + " and <= " + upper + " expected")
    val (right, in2) = mixfix_app(rprec(assoc, prec), in1, true)
    postinfix_app(lower, nprec(assoc, prec), binary(op, left, right), in2, cm)
  }

  def postinfix_app(
      lower: Int,
      upper: Int,
      left: Expr,
      in: Input[T],
      cm: Boolean
  ): Result[Expr, T] = {
    {
      infix_app(lower, upper, left, in, false)
    } or {
      postfix_app(lower, upper, left, in, false)
    } or {
      (left, in)
    }
  }

  def mixfix_arg(lower: Int, in: Input[T], cm: Boolean): Result[Expr, T] = {
    {
      prefix_app(lower, in, false)
    } or {
      inner_expr() parse (in, cm)
    }
  }

  def mixfix_app(lower: Int, in0: Input[T], cm: Boolean): Result[Expr, T] = {
    val (left, in1) = mixfix_arg(lower, in0, cm)
    postinfix_app(lower, max, left, in1, true)
  }

  def parse(in: Input[T], cm: Boolean) = {
    mixfix_app(min, in, cm)
  }

  def above(lower: Int) = new Parser[Expr, T]() {
    def format = name
    def parse(in: Input[T], cm: Boolean) = mixfix_app(lower, in, cm)
  }

  override def toString = {
    name
  }
}
