package wacc
package ast

import wacc.back._
import wacc.front.ParserBridge._

/* left values as a sealed trait with a position */
trait LValue {
    def pos: (Int, Int)
    def toAssembly(gen: CodeGenerator, table: Table): RegAssembly = TODOAssembly
}

/* pair elements as a sealed trait with a position */
sealed trait PairElem extends LValue with RValue {
    override def toAssembly(gen: CodeGenerator, table: Table): RegAssembly = {

        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(NullDereference)

        val (pair, offset) = this match {
            case Fst(x) => (x, 0)
            case Snd(x) => (x, gen.elemSize)
        }   
        
        val pairAss = pair.toAssembly(gen, table)
        val pairReg = pairAss.getReg
        val outAss = gen.regs.allocate
        val outReg = outAss.getReg

        val instrs = outAss.instr ++ pairAss.instr ++ Seq(Push(Register(8)), Mov(Register(8), pairReg), Cmp(Register(8), ImmInt(0)), LinkBranch("_errNull", Condition.EQ), Load(Register(8), Address(pairReg, ImmInt(offset))), Load(outReg, Address(Register(8), ImmInt(0))), Pop(Register(8)))
        // instrs :+ Cmp(pairReg, ImmInt(0))
        // instrs :+ LinkBranch("_errNull") //Error function needs to be defined
        // instrs :+ Load(outReg, Address(pairReg, ImmInt(offset)))

        return RegAssembly(outReg, instrs)
    }
}

case class Fst(x: LValue)(val pos: (Int, Int)) extends PairElem

object Fst extends ParserBridgePos1[LValue, Fst]

case class Snd(x: LValue)(val pos: (Int, Int)) extends PairElem

object Snd extends ParserBridgePos1[LValue, Snd]