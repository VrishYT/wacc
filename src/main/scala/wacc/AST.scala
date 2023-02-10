package wacc

import parsley.Parsley
import scala.language.implicitConversions
import ParserBridge._

object AST {
    import parsley.genericbridges._

    /* program case class with its functions and statements */
    case class Program(fs: List[Func], stats: List[Stat])

    /* program companion object with parser bridge */
    object Program extends ParserBridge2[List[Func], List[Stat], Program]

    /* function case class with position */
    case class Func(fs: (Type, String), args: List[Param], stats: List[Stat])(val pos: (Int, Int)) {

        /* define validReturn of a function, and match on the last statement : */
        def validReturn: Boolean = validReturn(stats)
        def validReturn(stats: List[Stat]): Boolean = stats.last match {

            /* return true if it's a return or exit */
            case _: Return | _: Exit => true

            /* check the other valid case for begin or if statements */
            case _ => {
                var valid = false 

                /* check if each branch of any new scope has a valid return, valid
                   will remain false if they don't */
                stats.foreach(stat => stat match {
                    case If(_, x, y) => {
                        valid |= validReturn(x) && validReturn(y)
                    }
                    case Begin(xs) => {
                        valid |= validReturn(xs)
                    }
                    case _ => 
                })

                /* return valid once all eligible paths of control flow
                   have been checked */
                valid
            }
        }
    }

    /* parameter case class with position */
    case class Param(t: Type, id: String)(val pos: (Int, Int))

    /* function and parameter companion objects with parser bridges */
    object Func extends ParserBridgePos3[(Type, String), List[Param], List[Stat], Func]
    object Param extends ParserBridgePos2[Type, String, Param]

    /* statements as objects extending the sealed trait Stat */
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

    /* parser bridges for statements */
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
    
    /* left values as a sealed trait with a position */
    sealed trait LValue {
        def pos: (Int, Int)
    }

    /* pair elements as a sealed trait with a position */
    sealed trait PairElem extends LValue with RValue
    case class Fst(x: LValue)(val pos: (Int, Int)) extends PairElem
    case class Snd(x: LValue)(val pos: (Int, Int)) extends PairElem

    object Fst extends ParserBridgePos1[LValue, Fst]
    object Snd extends ParserBridgePos1[LValue, Snd]

    /* right values as a sealed trait with a position attribute */
    sealed trait RValue {
        def pos: (Int, Int)
    }

    /* case classes for right values */
    case class ArrayLiteral(xs: List[Expr])(val pos: (Int, Int)) extends RValue
    case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends RValue
    case class Call(id: String, args: List[Expr])(val pos: (Int, Int)) extends RValue

    /* companion objects for right values */
    object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral]
    object NewPair extends ParserBridge2[Expr, Expr, NewPair] {
        def apply(fst: Expr, snd: Expr) = new NewPair(fst, snd)(fst.pos, snd.pos)
    }
    object Call extends ParserBridgePos2[String, List[Expr], Call]

    /* types */
    sealed trait Type

    /* case class for array type */
    case class ArrayType(t: Type) extends Type {

        /* override equals to also return true if either type is of AnyType */
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

    /* case class for pair types */
    case class PairType(fst: Type, snd: Type) extends Type {

        /* override equals to return true if being compared to a type deleted pair */
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

    /* companion objects for array and pair types, with parser bridges */
    object ArrayType extends ParserBridge1[Type, ArrayType]
    object PairType extends ParserBridge2[Type, Type, PairType]

    /* base types extending type */
    sealed trait BaseType extends Type

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
    case object AnyType extends BaseType {
        override def equals(that: Any): Boolean = true
    }

    /* the case object for a type deleted pair type */
    case object Pair extends Type with ParserBridge0[Type] {

        /* override equals so that a type deleted pair type can match any other pair type */
        override def equals(that: Any): Boolean = that match {
            case _: BaseType => false
            case _: ArrayType => false
            case _ => true
        }
    }

    /* expressions extending right values */
    sealed trait Expr extends RValue

    /* atomic types as case classes */
    case class IntLiteral(x: Int)(val pos: (Int, Int)) extends Expr
    case class CharLiteral(x: Char)(val pos: (Int, Int)) extends Expr
    case class StrLiteral(xs: String)(val pos: (Int, Int)) extends Expr
    case class BoolLiteral(x: Boolean)(val pos: (Int, Int)) extends Expr

    /* operators as case classes */
    case class UnaryOpExpr(op: UnaryOp, x: Expr)(val pos: (Int, Int)) extends Expr
    case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends Expr

    /* left expressions extending expressions and left values */
    sealed trait LExpr extends Expr with LValue {
        def pos: (Int, Int)
    }
    /* expressions extending left expressions */
    case class Ident(id: String)(val pos: (Int, Int)) extends LExpr
    case class ArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr
    case class IdentOrArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr

    /* expression parser bridges */
    object IntLiteral extends ParserBridgePos1[Int, IntLiteral]
    object CharLiteral extends ParserBridgePos1[Char, CharLiteral]
    object StrLiteral extends ParserBridgePos1[String, StrLiteral]
    object BoolLiteral extends ParserBridgePos1[Boolean, BoolLiteral]
    object Ident extends ParserBridgePos1[String, Ident]
    object ArrayElem extends ParserBridgePos2[String, List[Expr], ArrayElem]

    /* case class for a pair literal, (always null) */
    case class PairLiteralNull(val pos: (Int, Int)) extends Expr with ParserBridgePos0[Expr] 

    /* identifier or array elements with parser bridge */
    object IdentOrArrayElem extends ParserBridgePos2[String, Option[List[Expr]], LExpr] {
        def apply(id: String, xs: Option[List[Expr]])(pos: (Int, Int)): LExpr = xs match {
            case None => Ident(id)(pos)
            case Some(xs) => ArrayElem(id, xs)(pos)
        }
    }
    
    /* operators */
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

}


