package wacc
package ast

import wacc.back._
import wacc.front.ParserBridge._

/* left values as a sealed trait with a position */
trait LValue {
    def pos: (Int, Int)
    def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = TODOAssembly
}

/* pair elements as a sealed trait with a position */
sealed trait PairElem extends LValue with RValue {

    def getAddr(gen: CodeGenerator)(implicit table: Table): Assembly = {

        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(NullDereference)

        val (pair, offset) = this match {
            case Fst(x) => (x, 0)
            case Snd(x) => (x, gen.elemSize)
        }   
        
        val pairAss = pair.toAssembly(gen)
        val pairReg = Operands.opToReg(pairAss.getOp(), gen.regs)
        val accumAss = gen.regs.allocate
        val accumReg = accumAss.getReg()
        gen.regs.free(accumReg)

        val instrs = accumAss.instr ++ pairAss.instr ++ pairReg.instr ++ Seq(
                                                          Mov(accumReg, pairReg.getReg()),
                                                          Cmp(accumReg, ImmInt(0)),
                                                          LinkBranch("_errNull", Condition.EQ),
                                                          Load(accumReg, Address(pairReg.getReg(), ImmInt(offset)))
                                                          )

        return Assembly(Address(accumReg, ImmInt(0)), instrs)
    }

    override def toAssembly(gen: CodeGenerator)(implicit table: Table): RegAssembly = {

        val outAss = gen.regs.allocate
        val outReg = outAss.getReg()
        
        val assemb = this.getAddr(gen)

        return RegAssembly(outReg, outAss.instr ++ assemb.instr ++ Seq(Load(outReg, assemb.getOp())))
    }
}

case class Fst(x: LValue)(val pos: (Int, Int)) extends PairElem

object Fst extends ParserBridgePos1[LValue, Fst]

case class Snd(x: LValue)(val pos: (Int, Int)) extends PairElem

object Snd extends ParserBridgePos1[LValue, Snd]