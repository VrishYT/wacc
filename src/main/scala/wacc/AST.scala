object AST {
    // Program, functions and parameters
    case class Program(fs: List[Func], stats: List[Stat])

    case class Func(t: Type, id: String, args: List[Param], stats: List[Stat])
    case class Param(t: Type, id: String)

    // Statements
    sealed trait Stat
    case object Skip extends Stat
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
    case object IntType extends BaseType
    case object BoolType extends BaseType
    case object CharType extends BaseType
    case object StringType extends BaseType

    sealed trait PairElemType extends Type
    case object Pair extends PairElemType

    // Expressions
    sealed trait Expr extends RValue
    case class IntLiteral(x: Int) extends Expr
    case class CharLiteral(x: Char) extends Expr
    case class StrLiteral(xs: String) extends Expr
    case class BoolLiteral(x: Boolean) extends Expr
    case class ArrayElem(id: String, xs: List[Expr]) extends Expr with LValue
    case class UnaryOpExpr(op: UnaryOp, x: Expr) extends Expr
    case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr) extends Expr
    case class ParensExpr(x: Expr) extends Expr

    sealed trait PairLiteral extends Expr
    case object Null extends PairLiteral
    
    // Operators
    sealed trait UnaryOp
    case object Not extends UnaryOp
    case object Negate extends UnaryOp
    case object Length extends UnaryOp
    case object Ord extends UnaryOp
    case object Chr extends UnaryOp

    sealed trait BinaryOp
    case object Mul extends BinaryOp
    case object Div extends BinaryOp
    case object Mod extends BinaryOp
    case object Add extends BinaryOp
    // case object Add extends BinaryOp with ParserBridge2[Expr, Expr, Expr]
    // {
    //     def apply(x: Expr, y: Expr) = BinaryOpExpr(Add, x, y)
    // }
    case object Sub extends BinaryOp
    case object Greater extends BinaryOp
    case object GreaterThan extends BinaryOp
    case object Less extends BinaryOp
    case object LessThan extends BinaryOp
    case object Equal extends BinaryOp
    case object NotEqual extends BinaryOp
    case object And extends BinaryOp
    case object Or extends BinaryOp

    // Add(x, y)
    // BinaryOpExpr(Add, x, y)
}


