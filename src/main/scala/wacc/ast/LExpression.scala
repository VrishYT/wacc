package wacc
package ast

import wacc.front.ParserBridge._
import wacc.back._

/* left expressions extending expressions and left values */
sealed trait LExpr extends Expr with LValue {
    def pos: (Int, Int)
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): RegAssembly = TODOAssembly
}

/* expressions extending left expressions */
case class Ident(id: String)(val pos: (Int, Int)) extends LExpr {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): RegAssembly = Operands.opToReg(table.getOp(id), gen.regs)
}

object Ident extends ParserBridgePos1[String, Ident]

case class ArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): RegAssembly = {

        val outAss = gen.regs.allocate
        val outReg = outAss.getReg

        val reg1Ass = gen.regs.allocate
        val reg1 = reg1Ass.getReg

        val reg2Ass = gen.regs.allocate
        val reg2 = reg2Ass.getReg

        val arrAss = Operands.opToReg(table.getOp(id), gen.regs)
        val arrayOp = arrAss.getOp

        val first = xs.head
        val rest = xs.tail

        val firstAss = first.toAssembly(gen)
        val firstOp = firstAss.getOp

        val accAss = gen.regs.allocate
        val accReg = accAss.getReg

        gen.postSections.addOne(ArrayBoundsCheck)

        val instrns = reg1Ass.instr ++ reg2Ass.instr ++ outAss.instr ++ arrAss.instr
        instrns :+ firstAss.instr

        instrns :+ Mov(reg1, firstOp)
        instrns :+ Mov(reg2, arrayOp)
        instrns :+ LinkBranch("_arrload") // to define
        instrns :+ Mov(accReg, reg2)

        rest.foreach(x => {
          val xAss = x.toAssembly(gen)
          val op = xAss.getOp
          instrns :+ xAss.instr
          instrns :+ Push(accReg)
          instrns :+ Mov(reg1, op)
          instrns :+ Pop(accReg)
          instrns :+ Mov(reg2, accReg)
          instrns :+ LinkBranch("_arrload") // to define
          instrns :+ Mov(accReg, reg2)
          }
        )

        instrns :+ Mov(outReg, accReg)

        return RegAssembly(outReg, instrns)
    }
}
  
object ArrayElem extends ParserBridgePos2[String, List[Expr], ArrayElem]
  
case class IdentOrArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr

/* identifier or array elements with parser bridge */
object IdentOrArrayElem extends ParserBridgePos2[String, Option[List[Expr]], LExpr] {
    def apply(id: String, xs: Option[List[Expr]])(pos: (Int, Int)): LExpr = xs match {
        case None => Ident(id)(pos)
        case Some(xs) => ArrayElem(id, xs)(pos)
    }
}



