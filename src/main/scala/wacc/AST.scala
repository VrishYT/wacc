package wacc

import parsley.Parsley
import scala.language.implicitConversions

object AST {
    import parsley.genericbridges._

    // Program, functions and parameters
    case class Program(fs: List[Func], stats: List[Stat])

    object Program extends ParserBridge2[List[Func], List[Stat], Program]

    case class Func_(t: Type, id: String)
    case class Func(fs: Func_, args: List[Param], stats: List[Stat])
    case class Param(t: Type, id: String)

    object Func_ extends ParserBridge2[Type, String, Func_]
    object Func extends ParserBridge3[Func_, List[Param], List[Stat], Func]
    object Param extends ParserBridge2[Type, String, Param]

    // Statements
    sealed trait Stat
    case object Skip extends Stat with ParserBridge0[Stat]
    case class Declare(t: Type, id: String, rhs: RValue) extends Stat
    case class Assign(x: LValue, y: RValue) extends Stat
    case class Read(x: LValue) extends Stat
    case class Free(x: Expr) extends Stat
    case class Return(x: Expr) extends Stat
    case class Exit(x: Expr) extends Stat
    case class Print(x: Expr) extends Stat
    case class Println(x: Expr) extends Stat
    case class If(p: Expr, x: List[Stat], y: List[Stat]) extends Stat
    case class While(p: Expr, x: List[Stat]) extends Stat
    case class Begin(xs: List[Stat]) extends Stat

    object Declare extends ParserBridge3[Type, String, RValue, Declare]
    object Assign extends ParserBridge2[LValue, RValue, Assign]
    object Read extends ParserBridge1[LValue, Read]
    object Free extends ParserBridge1[Expr, Free]
    object Return extends ParserBridge1[Expr, Return]
    object Exit extends ParserBridge1[Expr, Exit]
    object Print extends ParserBridge1[Expr, Print]
    object Println extends ParserBridge1[Expr, Println]
    object If extends ParserBridge3[Expr, List[Stat], List[Stat], If]
    object While extends ParserBridge2[Expr, List[Stat], While]
    object Begin extends ParserBridge1[List[Stat], Begin]
    
    // Left and Right Values
    sealed trait LValue

    sealed trait PairElem extends LValue with RValue
    case class Fst(x: LValue) extends PairElem
    case class Snd(x: LValue) extends PairElem

    object Fst extends ParserBridge1[LValue, Fst]
    object Snd extends ParserBridge1[LValue, Snd]

    sealed trait RValue
    case class ArrayLiteral(xs: List[Expr]) extends RValue
    case class NewPair(fst: Expr, snd: Expr) extends RValue
    case class Call(id: String, args: List[Expr]) extends RValue
    
    object ArrayLiteral extends ParserBridge1[List[Expr], ArrayLiteral]
    object NewPair extends ParserBridge2[Expr, Expr, NewPair]
    object Call extends ParserBridge2[String, List[Expr], Call]

    // Types
    sealed trait Type
    case object AnyType extends Type
    case class ArrayType(t: Type) extends Type with PairElemType {

        override def equals(that: Any): Boolean = {
            that match {
                case ArrayType(t) => {
                    if (this.t == AnyType || t == AnyType) {
                        return true
                    } else {
                        return this.t == t
                    }
                }
                case _ => return false
            }
        }

    }
    case class PairType(fst: PairElemType, snd: PairElemType) extends Type with PairElemType {

        override def equals(that: Any): Boolean = {
            def pairEq(p: PairType) = {
                this.fst == p.fst && this.snd == p.snd
            }
            this match {
                case PairType (null, null) => return true
                case x: PairType => pairEq(x)
            }
            that match {
                case PairType(null, null) => return true
                case x: PairType => pairEq(x)
                case _ => return false
            }
        }

    }

    object ArrayType extends ParserBridge1[Type, ArrayType]
    object PairType extends ParserBridge2[PairElemType, PairElemType, PairType]

    sealed trait BaseType extends Type with PairElemType
    case object IntType extends BaseType with ParserBridge0[BaseType] {
        def apply() = IntType
    }
    case object BoolType extends BaseType with ParserBridge0[BaseType] {
        def apply() = BoolType
    }
    case object CharType extends BaseType with ParserBridge0[BaseType] {
        def apply() = CharType
    }
    case object StringType extends BaseType with ParserBridge0[BaseType] {
        def apply() = StringType
    }

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
    
    object IntLiteral extends ParserBridge1[Int, IntLiteral]
    object CharLiteral extends ParserBridge1[Char, CharLiteral]
    object StrLiteral extends ParserBridge1[String, StrLiteral]
    object BoolLiteral extends ParserBridge1[Boolean, BoolLiteral]
    object Ident extends ParserBridge1[String, Ident]
    object ArrayElem extends ParserBridge2[String, List[Expr], ArrayElem]

    case object PairLiteralNull extends Expr with ParserBridge0[Expr] 
    
    // Operators
    sealed trait UnaryOp
    case object Not extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Not, x)
    }
    case object Negate extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Negate, x)
    }
    case object Length extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Length, x)
    }
    case object Ord extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Ord, x)
    }
    case object Chr extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
        def apply(x: Expr) = UnaryOpExpr(Chr, x)
    }
    
    sealed trait BinaryOp
    case object Mul extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Mul, x, y)
    }
    case object Div extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Div, x, y)
    }
    case object Mod extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Mod, x, y)
    }
    case object Add extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Add, x, y)
    }
    case object Sub extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Sub, x, y)
    }
    case object Greater extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Greater, x, y)
    }
    case object GreaterEquals extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(GreaterEquals, x, y)
    }
    case object Less extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Less, x, y)
    }
    case object LessEquals extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(LessEquals, x, y)
    }
    case object Equal extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Equal, x, y)
    }
    case object NotEqual extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(NotEqual, x, y)
    }
    case object And extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(And, x, y)
    }
    case object Or extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr) = BinaryOpExpr(Or, x, y)
    }

}


