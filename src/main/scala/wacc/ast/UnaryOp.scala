package wacc
package ast

import parsley.genericbridges._ 

sealed abstract class UnaryOp(val input: Type, val output: Type) 

case object Not extends UnaryOp(BoolType, BoolType) with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Not, x)(x.pos)
}

case object Negate extends UnaryOp(IntType, IntType) with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Negate, x)(x.pos)
}

case object Length extends UnaryOp(ArrayType(AnyType), IntType) with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Length, x)(x.pos)
}

case object Ord extends UnaryOp(CharType, IntType) with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Ord, x)(x.pos)
}

case object Chr extends UnaryOp(IntType, CharType) with ParserBridge1[Expr, UnaryOpExpr] {
  def apply(x: Expr) = UnaryOpExpr(Chr, x)(x.pos)
}
