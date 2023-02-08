package wacc

import parsley.Parsley
import scala.language.implicitConversions
import ParserBridge._

object AST {
    import parsley.genericbridges._

    // Program, functions and parameters
    case class Program(fs: List[Func], stats: List[Stat])(val pos: (Int, Int))

    object Program extends ParserBridgePos2[List[Func], List[Stat], Program]

    case class Func(fs: (Type, String), args: List[Param], stats: List[Stat])(val pos: (Int, Int)) {
        def validReturn: Boolean = validReturn(stats)
        def validReturn(stats: List[Stat]): Boolean = stats.last match {
            case _: Return | _: Exit => true
            case _ => {
                var valid = false 
                stats.foreach(stat => stat match {
                    case If(_, x, y) => {
                        valid |= validReturn(x) && validReturn(y)
                    }
                    case Begin(xs) => {
                        valid |= validReturn(xs)
                    }
                    case _ => 
                })
                valid
            }
        }
    }
    case class Param(t: Type, id: String)(val pos: (Int, Int))

    object Func extends ParserBridgePos3[(Type, String), List[Param], List[Stat], Func]
    object Param extends ParserBridgePos2[Type, String, Param]

    // Statements
    sealed trait Stat
    case object Skip extends Stat with ParserBridge0[Stat]
    case class Declare(t: Type, id: String, rhs: RValue)(val pos: (Int, Int)) extends Stat
    case class Assign(x: LValue, y: RValue)(val pos: (Int, Int)) extends Stat
    case class Read(x: LValue)(val pos: (Int, Int)) extends Stat
    case class Free(x: Expr)(val pos: (Int, Int)) extends Stat
    case class Return(x: Expr)(val pos: (Int, Int)) extends Stat
    case class Exit(x: Expr)(val pos: (Int, Int)) extends Stat
    case class Print(x: Expr)(val pos: (Int, Int)) extends Stat
    case class Println(x: Expr)(val pos: (Int, Int)) extends Stat
    case class If(p: Expr, x: List[Stat], y: List[Stat])(val pos: (Int, Int)) extends Stat
    case class While(p: Expr, x: List[Stat])(val pos: (Int, Int)) extends Stat
    case class Begin(xs: List[Stat])(val pos: (Int, Int)) extends Stat

    object Declare extends ParserBridgePos3[Type, String, RValue, Declare]
    object Assign extends ParserBridgePos2[LValue, RValue, Assign]
    object Read extends ParserBridgePos1[LValue, Read]
    object Free extends ParserBridgePos1[Expr, Free]
    object Return extends ParserBridgePos1[Expr, Return]
    object Exit extends ParserBridgePos1[Expr, Exit]
    object Print extends ParserBridgePos1[Expr, Print]
    object Println extends ParserBridgePos1[Expr, Println]
    object If extends ParserBridgePos3[Expr, List[Stat], List[Stat], If]
    object While extends ParserBridgePos2[Expr, List[Stat], While]
    object Begin extends ParserBridgePos1[List[Stat], Begin]
    
    // Left and Right Values
    sealed trait LValue

    sealed trait PairElem extends LValue with RValue
    case class Fst(x: LValue)(val pos: (Int, Int)) extends PairElem
    case class Snd(x: LValue)(val pos: (Int, Int)) extends PairElem

    object Fst extends ParserBridgePos1[LValue, Fst]
    object Snd extends ParserBridgePos1[LValue, Snd]

    sealed trait RValue
    case class ArrayLiteral(xs: List[Expr])(val pos: (Int, Int)) extends RValue
    case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int)) extends RValue
    case class Call(id: String, args: List[Expr])(val pos: (Int, Int)) extends RValue
    
    object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral]
    object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]
    object Call extends ParserBridgePos2[String, List[Expr], Call]

    // Types
    sealed trait Type
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
            that match {
                case x: PairType => pairEq(x)
                case x: BaseType => false
                case Pair => true
                case _ => false
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
    case object AnyType extends BaseType with PairElemType {
        override def equals(that: Any): Boolean = true
    }

    sealed trait PairElemType extends Type
    case object Pair extends PairElemType with ParserBridge0[PairElemType] {
        override def equals(that: Any): Boolean = that match {
            case _: BaseType => false
            case _: ArrayType => false
            case _ => true
        }
    }

    // Expressions
    sealed trait Expr extends RValue
    case class IntLiteral(x: Int)(val pos: (Int, Int)) extends Expr
    case class CharLiteral(x: Char)(val pos: (Int, Int)) extends Expr
    case class StrLiteral(xs: String)(val pos: (Int, Int)) extends Expr
    case class BoolLiteral(x: Boolean)(val pos: (Int, Int)) extends Expr
    
    case class UnaryOpExpr(op: UnaryOp, x: Expr)(val pos: (Int, Int)) extends Expr
    case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr

    sealed trait LExpr extends Expr with LValue
    case class Ident(id: String) extends LExpr
    case class ArrayElem(id: String, xs: List[Expr]) extends LExpr
    case class IdentOrArrayElem(id: String, xs: List[Expr]) extends LExpr
    
    object IntLiteral extends ParserBridgePos1[Int, IntLiteral]
    object CharLiteral extends ParserBridgePos1[Char, CharLiteral]
    object StrLiteral extends ParserBridgePos1[String, StrLiteral]
    object BoolLiteral extends ParserBridgePos1[Boolean, BoolLiteral]
    object Ident extends ParserBridge1[String, Ident]
    object ArrayElem extends ParserBridge2[String, List[Expr], ArrayElem]

    case object PairLiteralNull extends Expr with ParserBridge0[Expr]

    object IdentOrArrayElem extends ParserBridge2[String, Option[List[Expr]], LExpr] {
        def apply(id: String, xs: Option[List[Expr]]): LExpr = xs match {
            case None => Ident(id)
            case Some(xs) => ArrayElem(id, xs)
        }
    }
    
    // Operators
    sealed trait UnaryOp
    case object Not extends UnaryOp with ParserBridgePos1[Expr, UnaryOpExpr] {
        def apply(x: Expr)(pos: (Int, Int)) = UnaryOpExpr(Not, x)(pos)
    }
    case object Negate extends UnaryOp with ParserBridgePos1[Expr, UnaryOpExpr] {
        def apply(x: Expr)(pos: (Int, Int)) = UnaryOpExpr(Negate, x)(pos)
    }
    case object Length extends UnaryOp with ParserBridgePos1[Expr, UnaryOpExpr] {
        def apply(x: Expr)(pos: (Int, Int)) = UnaryOpExpr(Length, x)(pos)
    }
    case object Ord extends UnaryOp with ParserBridgePos1[Expr, UnaryOpExpr] {
        def apply(x: Expr)(pos: (Int, Int)) = UnaryOpExpr(Ord, x)(pos)
    }
    case object Chr extends UnaryOp with ParserBridgePos1[Expr, UnaryOpExpr] {
        def apply(x: Expr)(pos: (Int, Int)) = UnaryOpExpr(Chr, x)(pos)
    }
    
    sealed trait BinaryOp
    case object Mul extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(Mul, x, y)(pos)
    }
    case object Div extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(Div, x, y)(pos)
    }
    case object Mod extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(Mod, x, y)(pos)
    }
    case object Add extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(Add, x, y)(pos)
    }
    case object Sub extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(Sub, x, y)(pos)
    }
    case object Greater extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(Greater, x, y)(pos)
    }
    case object GreaterEquals extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(GreaterEquals, x, y)(pos)
    }
    case object Less extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(Less, x, y)(pos)
    }
    case object LessEquals extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(LessEquals, x, y)(pos)
    }
    case object Equal extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(Equal, x, y)(pos)
    }
    case object NotEqual extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(NotEqual, x, y)(pos)
    }
    case object And extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(And, x, y)(pos)
    }
    case object Or extends BinaryOp with ParserBridgePos2[Expr, Expr, BinaryOpExpr] {
        def apply(x: Expr, y: Expr)(pos: (Int, Int)) = BinaryOpExpr(Or, x, y)(pos)
    }

}


