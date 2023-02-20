package wacc.ast

import wacc.back._
import wacc.front.ParserBridge._

/* left values as a sealed trait with a position */
trait LValue {
    def pos: (Int, Int)
    def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): RegAssembly = TODOAssembly
}

/* pair elements as a sealed trait with a position */
sealed trait PairElem extends LValue with RValue {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): RegAssembly = TODOAssembly
}

case class Fst(x: LValue)(val pos: (Int, Int)) extends PairElem

object Fst extends ParserBridgePos1[LValue, Fst]

case class Snd(x: LValue)(val pos: (Int, Int)) extends PairElem

object Snd extends ParserBridgePos1[LValue, Snd]