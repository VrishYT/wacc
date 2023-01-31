package wacc

import parsley.Parsley
import scala.language.implicitConversions

object AST {
    import GenericBridges._

    // Program, functions and parameters
    case class Program(fs: List[Func], stats: List[Stat])

    case class Func(t: Type, id: String, args: List[Param], stats: List[Stat])
    case class Param(t: Type, id: String)

    // Statements
    sealed trait Stat
    case object Skip extends Stat with ParserBridge0[Stat]
    case class Declare(t: Type, id: String, rhs: RValue) extends Stat
    case class Assign(x: LValue, y: RValue) extends Stat
    case class Read(x: LValue) extends Stat
    case class Free(x: Expr) extends Stat
    case class Return(x: Expr) extends Stat
    case class Exit(x: Expr) extends Stat
    case class Print(x: Expr, end: Char) extends Stat
    case class If(p: Expr, x: List[Stat], y: List[Stat]) extends Stat
    case class While(p: Expr, x: List[Stat]) extends Stat
    case class Begin(xs: List[Stat]) extends Stat
    
    // Left and Right Values
    sealed trait LValue

    sealed trait PairElem extends LValue with RValue
    case class Fst(x: LValue) extends PairElem
    case class Snd(x: LValue) extends PairElem

    sealed trait RValue
    case class ArrayLiteral(xs: List[Expr]) extends RValue
    case class NewPair(fst: Expr, snd: Expr) extends RValue
    case class Call(id: String, args: List[Expr]) extends RValue

    // Types
    sealed trait Type
    case class ArrayType(t: Type) extends Type with PairElemType
    case class PairType(fst: PairElemType, snd: PairElemType) extends Type

    sealed trait BaseType extends Type with PairElemType
    case object IntType extends BaseType with ParserBridge0[BaseType]
    case object BoolType extends BaseType with ParserBridge0[BaseType]
    case object CharType extends BaseType with ParserBridge0[BaseType]
    case object StringType extends BaseType with ParserBridge0[BaseType]

    sealed trait PairElemType extends Type
    case object Pair extends PairElemType with ParserBridge0[PairElemType]
    
    // Expressions
    sealed trait Expr extends RValue
    case class IntLiteral(x: Int) extends Expr
    case class CharLiteral(x: Char) extends Expr
    case class StrLiteral(xs: String) extends Expr
    case class BoolLiteral(x: Boolean) extends Expr
    case class Ident(id: String) extends Expr with LValue
    case class ArrayElem(id: String, xs: List[Expr]) extends Expr with LValue
    case class UnaryOpExpr(op: UnaryOp, x: Expr) extends Expr
    case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr) extends Expr

    sealed trait PairLiteral extends Expr
    case object Null extends PairLiteral with ParserBridge0[PairLiteral] 
    
    // Operators
    sealed trait UnaryOp
    case object Not extends UnaryOp
        with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Not, x)
    }
    case object Negate extends UnaryOp
        with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Negate, x)
    }
    case object Length extends UnaryOp
        with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Length, x)
    }
    case object Ord extends UnaryOp
        with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Ord, x)
    }
    case object Chr extends UnaryOp
        with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Chr, x)
    }

    sealed trait BinaryOp
    case object Mul extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Mul, x, y)
    }
    case object Div extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Div, x, y)
    }
    case object Mod extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Mod, x, y)
    }
    case object Add extends BinaryOp 
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Add, x, y)
    }
    case object Sub extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Sub, x, y)
    }
    case object Greater extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Greater, x, y)
    }
    case object GreaterEquals extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(GreaterEquals, x, y)
    }
    case object Less extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Less, x, y)
    }
    case object LessEquals extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(LessEquals, x, y)
    }
    case object Equal extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Equal, x, y)
    }
    case object NotEqual extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(NotEqual, x, y)
    }
    case object And extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(And, x, y)
    }
    case object Or extends BinaryOp
        with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Or, x, y)
    }
}


