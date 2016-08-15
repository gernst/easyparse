package arse.ebnf

trait Expr

case class Tok(text: String) extends Expr {
  override def toString = text
}

case class Id(name: String) extends Expr {
  override def toString = name
}

case class Named(name: Id, expr: Expr) extends Expr {
  override def toString = name + "=" + expr
}

case class Opt(expr: Expr) extends Expr {
  override def toString = expr + "?"
}

case class Rep(expr: Expr, isPlus: Boolean) extends Expr {
  override def toString = {
    expr + (if(isPlus) "+" else "*")
  }
}

case class Alt(expr1: Expr, expr2: Expr) extends Expr {
  override def toString = expr1 + " | " + expr2
}

case class Seq(exprs: List[Expr]) extends Expr {
  override def toString = exprs match {
    case List(inner: Seq) => inner.toString
    case _ => exprs.mkString("(", " ", ")")
  }
}