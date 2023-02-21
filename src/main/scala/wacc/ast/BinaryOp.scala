package wacc
package ast

import parsley.genericbridges._

sealed abstract class BinaryOp(val input: Seq[Type], val output: Type) 

case object Mul extends BinaryOp(Seq(IntType), IntType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Mul, x, y)(x.pos, y.pos)
}

case object Div extends BinaryOp(Seq(IntType), IntType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Div, x, y)(x.pos, y.pos)
}

case object Mod extends BinaryOp(Seq(IntType), IntType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Mod, x, y)(x.pos, y.pos)
}

case object Add extends BinaryOp(Seq(IntType), IntType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Add, x, y)(x.pos, y.pos)
}

case object Sub extends BinaryOp(Seq(IntType), IntType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Sub, x, y)(x.pos, y.pos)
}

case object Greater extends BinaryOp(Seq(CharType, IntType), BoolType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Greater, x, y)(x.pos, y.pos)
}

case object GreaterEquals extends BinaryOp(Seq(CharType, IntType), BoolType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(GreaterEquals, x, y)(x.pos, y.pos)
}

case object Less extends BinaryOp(Seq(CharType, IntType), BoolType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Less, x, y)(x.pos, y.pos)
}

case object LessEquals extends BinaryOp(Seq(CharType, IntType), BoolType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(LessEquals, x, y)(x.pos, y.pos)
}

case object Equal extends BinaryOp(Seq(AnyType), BoolType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Equal, x, y)(x.pos, y.pos)
}

case object NotEqual extends BinaryOp(Seq(AnyType), BoolType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(NotEqual, x, y)(x.pos, y.pos)
}

case object And extends BinaryOp(Seq(BoolType), BoolType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(And, x, y)(x.pos, y.pos)
}

case object Or extends BinaryOp(Seq(BoolType), BoolType) with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Or, x, y)(x.pos, y.pos)
}