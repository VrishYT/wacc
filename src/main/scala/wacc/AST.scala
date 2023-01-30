object AST {
    // Program, functions and parameters
    case class Program(fs: List[Func], stats: List[Stat])

    case class Func(t: Type, id: Ident, args: List[Param], stats: List[Stat])
    case class Param(t: Type, id: Ident)

    // Statements
    sealed trait Stat
    case object Skip extends Stat
    case class Declare(t: Type, id: Ident, rhs: RValue) extends Stat
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
    case class Call(id: Ident, args: List[Expr]) extends RValue

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
    case class IntLiteral(sgn: IntSign, x: List[Digit]) extends Expr
    case class CharLiteral(x: CharLetter) extends Expr
    case class StrLiteral(xs: List[CharLetter]) extends Expr
    case class Ident(v: String) extends Expr with LValue
    case class ArrayElem(id: Ident, xs: [Expr]) extends Expr with LValue
    case class UnaryOpExpr(op: UnaryOp, x: Expr) extends Expr
    case class BinaryOpExpr(x: Expr, op: BinaryOp, y: Expr) extends Expr
    case class ParensExpr(x: Expr) extends Expr
    
    sealed trait BoolLiteral extends Expr
    case object True extends BoolLiteral
    case object False extends BoolLiteral

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
    case object Sub extends BinaryOp
    case object Greater extends BinaryOp
    case object GreaterThan extends BinaryOp
    case object Less extends BinaryOp
    case object LessThan extends BinaryOp
    case object Equal extends BinaryOp
    case object NotEqual extends BinaryOp
    case object And extends BinaryOp
    case object Or extends BinaryOp

    // Integer Signs
    sealed trait IntSign
    case object Positive extends IntSign
    case object Negative extends IntSign

    // Digits
    sealed trait Digit
    case object Zero extends Digit
    case object One extends Digit
    case object Two extends Digit
    case object Three extends Digit
    case object Four extends Digit
    case object Five extends Digit
    case object Six extends Digit
    case object Seven extends Digit
    case object Eight extends Digit
    case object Nine extends Digit

    // Characters
    sealed trait CharLetter
    case class NormalChar(x: Char) extends CharLetter
    
    sealed trait EscapedChar extends CharLetter
    case object NullTerminator extends EscapedChar
    case object Backspace extends EscapedChar
    case object HorizontalTab extends EscapedChar
    case object Newline extends EscapedChar
    case object Formfeed extends EscapedChar
    case object CarriageReturn extends EscapedChar
    case object DoubleQuote extends EscapedChar
    case object SingleQuote extends EscapedChar
    case object Backslash extends EscapedChar

    // Comments
    case class Comment(x: String)




}


