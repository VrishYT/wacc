package wacc.ast

import parsley.genericbridges._ 

sealed trait UnaryOp

case object Not extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Not, x)(x.pos)
}

case object Negate extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Negate, x)(x.pos)
}

case object Length extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Length, x)(x.pos)
}

case object Ord extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Ord, x)(x.pos)
}

case object Chr extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Chr, x)(x.pos)
}
