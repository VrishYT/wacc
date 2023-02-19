package wacc.ast

import wacc.back._
import wacc.front.ParserBridge._
import parsley.genericbridges._

/* right values as a sealed trait with a position attribute */
trait RValue {
    def pos: (Int, Int)
    def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Assembly = Assembly(ImmInt(0))
}

/* case classes for right values */
case class ArrayLiteral(xs: List[Expr])(val pos: (Int, Int)) extends RValue

/* companion objects for right values */
object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral]

case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends RValue

object NewPair extends ParserBridge2[Expr, Expr, NewPair] {
    def apply(fst: Expr, snd: Expr) = new NewPair(fst, snd)(fst.pos, snd.pos)
}

case class Call(id: String, args: List[Expr])(val pos: (Int, Int)) extends RValue

object Call extends ParserBridgePos2[String, List[Expr], Call]