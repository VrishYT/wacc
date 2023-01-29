object AST {
    case class Program(fs: List[Func], stats: List[Stat])

    case class Func(t: Type, id: Ident, args: List[Param], stats: List[Stat])
    case class Param(t: Type, id: Ident)

    sealed trait Stat
    case object Skip extends Stat
    case class Declare(t: Type, id: Ident, rhs: RValue) extends Stat
    case class Assign(lhs: LValue, y: RValue) extends Stat
    case class Read(x: LValue) extends Stat
    case class Free(x: Expr) extends Stat
    case class Return(x: Expr) extends Stat
    case class Exit(x: Expr) extends Stat
    case class Print(x: Expr, end: Char) extends Stat
    case class If(p: Expr, x: Stat, y: Stat) extends Stat
    case class While(p: Expr, x: Stat) extends Stat
    case class Begin(x: Stat) extends Stat

    
    sealed trait Expr extends Stat
    case class IntLiteral(sgn: IntSign, x: Digits) extends Expr
    case class CharLiteral(x: CharLetter) extends Expr
    case class StrLiteral(xs: List[CharLetter]) extends Expr
    case class UnaryOpExpr(op: UnaryOp, x: Expr) extends Expr
    case class BinaryOpExpr(x: Expr, op: BinaryOp, y: Expr) extends Expr
    case class ParensExpr(x: Expr) extends Expr

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

    sealed trait Type
    sealed trait PairElemType extends Type
    case object PairKeyword extends PairElemType
    case class PairType(fst: PairElemType, snd: PairElemType) extends Type
    sealed trait BaseType extends PairElemType
    case object IntType extends BaseType
    case object BoolType extends BaseType
    case object CharType extends BaseType
    case object StringType extends BaseType
    case class ArrayType(t: Type) extends PairElemType

    // sealed trait Expr1 extends Expr
    
    // sealed trait Ident extends Expr1
    // case class Ident_(v: String) extends Ident
    
    // sealed trait ArrayElem extends Expr1
    // case class ArrayElem_(id: Ident, dims: [Expr]) extends ArrayElem

    // sealed trait LValue
    // case class (x: Expr1)
    
    sealed trait PairElem extends LValue
    case class Fst(x: LValue) extends PairElem
    case class Snd(x: LValue) extends PairElem

    sealed trait RValue


    sealed trait IntSign extends IntLiteral
    case object Positive extends IntSign
    case object Negative extends IntSign

    sealed trait BoolLiteral extends Expr
    case object True extends BoolLiteral
    case object False extends BoolLiteral

    sealed trait PairLiteral extends Expr
    case object Null extends PairLiteral

    sealed trait CharLetter
    case class Escaped(x: Character)


    // case class ArrayLiteral(xs: List[Expr]) extends RValue




}


