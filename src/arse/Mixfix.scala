// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

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
}

trait Mixfix[T, O, E] extends Parser[T, E] {
  import Mixfix._
  import Parser._

  def inner_expr: Parser[T, E]
  def apply(op: O, args: List[E]): E

  def prefix_op: Parser[T, (O, Int)]
  def postfix_op: Parser[T, (O, Int)]
  def infix_op: Parser[T, (O, (Assoc, Int))]

  def unary(op: O, arg: E) = apply(op, List(arg))
  def binary(op: O, arg1: E, arg2: E) = apply(op, List(arg1, arg2))

  def prefix_app(lower: Int, in0: List[T]) = {
    val ((op, prec), in1) = prefix_op(in0)
    if (prec < lower) fail
    val (right, in2) = mixfix_app(prec, in1)
    (unary(op, right), in2)
  }

  def postfix_app(lower: Int, upper: Int, left: E, in0: List[T]) = {
    val ((op, prec), in1) = postfix_op(in0)
    if (prec < lower || upper < prec) fail
    postinfix_app(lower, prec, unary(op, left), in1)
  }

  def infix_app(lower: Int, upper: Int, left: E, in0: List[T]) = {
    val ((op, (assoc, prec)), in1) = infix_op(in0)
    if (prec < lower || upper < prec) fail
    val (right, in2) = mixfix_app(rprec(assoc, prec), in1)
    postinfix_app(lower, nprec(assoc, prec), binary(op, left, right), in2)
  }

  def postinfix_app(lower: Int, upper: Int, left: E, in: List[T]): (E, List[T]) = {
    {
      infix_app(lower, upper, left, in)
    } or {
      postfix_app(lower, upper, left, in)
    } or {
      (left, in)
    }
  }

  def mixfix_arg(lower: Int, in: List[T]) = {
    {
      prefix_app(lower, in)
    } or {
      inner_expr(in)
    }
  }

  def mixfix_app(lower: Int, in0: List[T]): (E, List[T]) = {
    val (left, in1) = mixfix_arg(lower, in0)
    postinfix_app(lower, Max, left, in1)
  }

  def apply(in: List[T]) = mixfix_app(Min, in)
}

object Mixfix {
  import Parser._
  import Recognizer._

  def nprec(assoc: Assoc, prec: Int) = if (assoc == Left) prec else prec - 1
  def rprec(assoc: Assoc, prec: Int) = if (assoc == Right) prec else prec + 1

  def Min = Int.MinValue
  def Max = Int.MaxValue

  def mixfix[T, O, E](p: => Parser[T, E], op: T => O, ap: (O, List[E]) => E, s: Syntax[T]): Parser[T, E] = new Mixfix[T, O, E]() {
    lazy val inner_expr = p
    def apply(op: O, args: List[E]) = ap(op, args)

    val prefix_op = mixfix_op(s.prefix_ops, op)
    val postfix_op = mixfix_op(s.postfix_ops, op)
    val infix_op = mixfix_op(s.infix_ops, op)
  }

  def mixfix_op[T, O, A](m: Map[T, A], op: T => O): Parser[T, (O, A)] = parse {
    case a :: in if m contains a => ((op(a), m(a)), in)
    case _                       => fail
  }

  def mixfix_op[T, O](s: Set[T], op: T => O): Parser[T, O] = parse {
    case a :: in if s contains a => (op(a), in)
    case _                       => fail
  }
}