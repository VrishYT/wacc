package wacc
package ast

import parsley.genericbridges._

sealed trait BinaryOp

case object Mul extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Mul, x, y)(x.pos, y.pos)
}

case object Div extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Div, x, y)(x.pos, y.pos)
}

case object Mod extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Mod, x, y)(x.pos, y.pos)
}

case object Add extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Add, x, y)(x.pos, y.pos)
}

case object Sub extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Sub, x, y)(x.pos, y.pos)
}

case object Greater extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Greater, x, y)(x.pos, y.pos)
}

case object GreaterEquals extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(GreaterEquals, x, y)(x.pos, y.pos)
}

case object Less extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Less, x, y)(x.pos, y.pos)
}

case object LessEquals extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(LessEquals, x, y)(x.pos, y.pos)
}

case object Equal extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Equal, x, y)(x.pos, y.pos)
}

case object NotEqual extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(NotEqual, x, y)(x.pos, y.pos)
}

case object And extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(And, x, y)(x.pos, y.pos)
}

case object Or extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Or, x, y)(x.pos, y.pos)
}