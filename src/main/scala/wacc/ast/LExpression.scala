package wacc
package ast

import wacc.front.ParserBridge._
import wacc.back._
import scala.collection.mutable.ListBuffer

/* left expressions extending expressions and left values */
sealed trait LExpr extends Expr with LValue {
  def pos: (Int, Int)
  override def toAssembly(gen: CodeGenerator, table: Table): RegAssembly = TODOAssembly
}

/* expressions extending left expressions */
case class Ident(id: String)(val pos: (Int, Int)) extends LExpr {
  override def toAssembly(gen: CodeGenerator, table: Table): RegAssembly = RegAssembly(gen.regs.get(id))
}

object Ident extends ParserBridgePos1[String, Ident]

case class ArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr {
  override def toAssembly(gen: CodeGenerator, table: Table): RegAssembly = {

        val outAss = gen.regs.allocate
        val outReg = outAss.getReg

        val reg1Ass = gen.regs.allocate
        val reg1 = reg1Ass.getReg

        val reg2Ass = gen.regs.allocate
        val reg2 = reg2Ass.getReg

        val arrAss = RegAssembly(gen.regs.get(id))
        val arrayOp = arrAss.getOp

        val first = xs.head
        val rest = xs.tail

        val firstAss = first.toAssembly(gen, table)
        val firstOp = firstAss.getOp

        val accAss = gen.regs.allocate
        val accReg = accAss.getReg

        gen.postSections.addOne(ArrayBoundsCheck)

        val startInstrs: Seq[Instruction] = reg1Ass.instr ++ reg2Ass.instr ++ outAss.instr ++ arrAss.instr ++ firstAss.instr ++ Seq(
          Mov(reg1, firstOp),
          Mov(reg2, arrayOp),
          LinkBranch("_arrload"), // to define
          Mov(accReg, reg2)
        )

        val restInstrs: Seq[Instruction] = rest.map(x => {
          val xAss = x.toAssembly(gen, table)
          val op = xAss.getOp
          xAss.instr ++ Seq(
            Push(accReg),
            Mov(reg1, op),
            Pop(accReg),
            Mov(reg2, accReg),
            LinkBranch("_arrload"), // to define
            Mov(accReg, reg2)
          )
        }).flatten

        val finalInstrs = Seq(Mov(outReg, accReg))

        return RegAssembly(outReg, startInstrs ++ restInstrs ++ finalInstrs)
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



