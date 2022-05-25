package arse

import scala.util.DynamicVariable

object Test {
  sealed trait Type
  case class Sort(name: String) extends Type

  case class Fun(name: String, args: List[Type], res: Type)

  sealed trait Quant
  case object Exists extends Quant
  case object Forall extends Quant

  sealed trait Expr
  case class Var(name: String, typ: Type) extends Expr
  case class App(fun: Fun, args: List[Expr]) extends Expr
  case class Bind(quant: Quant, bound: List[Var], body: Expr) extends Expr

  object App {
    val from: ((String, Option[List[Expr]]) => Expr) = {
      case (id, None) =>
        scope lookup id
      case (id, Some(args)) =>
        App(sig lookup id, args)
    }
  }

  object Var {
    val from: ((String, Type) => Var) = { case (name, typ) =>
      val x = Var(name, typ)
      scope.extend(x)
      x
    }
  }

  import implicits._

  def parens[A](p: Parser[A, Token]) = "(" ~ p ~ ")"

  object sig extends DynamicVariable[Map[String, Fun]](Map.empty) {
    def lookup(id: String) = value(id)
  }

  object scope extends DynamicVariable[Map[String, Var]](Map.empty) {
    def lookup(id: String) =
      value(id)

    def extend(x: Var) =
      value = value + (x.name -> x)

    def here[A, T](p: Parser[A, T]) =
      Parser.Scoped(p, this)
  }

  val id = V[String]("id")

  val typ = P(Sort(id))

  val expr: Parser[Expr, Token] =
    P(parens(expr) | bind | app)

  val args = P(parens(expr ~* ","))
  val app = P(App.from(id ~ args.?))

  val quant = P(Forall("forall") | Exists("exists"))
  val formal = P(Var.from(id ~ ":" ~ typ))
  val formals = P(formal ~* ",")
  val bind = P(Bind(scope.here(quant ~ formals ~ "." ~ expr)))

  def main(args: Array[String]) {
    val x = id.Result("x")
    val y = id.Result("y")
    val int = id.Result("int")

    val in = List[Token](
      "forall",
      x,
      ":",
      int,
      ".",
      x
    )

    val e = expr.parseAll(in)
    println(e)
  }
}
