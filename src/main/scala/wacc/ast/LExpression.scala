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
  
// ARGUMENT 1: ptr, ARGUMENT 2: index, FOR _arrLoad
// ARGUMENT 1: ptr, ARGUMENT 2: index, ARGUMENT 3: value, FOR _arrStore
  override def toAssembly(gen: CodeGenerator)(implicit table: Table): RegAssembly = {
    return toAssemblyLoad(gen)
  }

  def toAssemblyLoad(gen: CodeGenerator)(implicit table: Table): RegAssembly = {
    
    gen.postSections.addOne(ArrayBoundsCheck)
    gen.postSections.addOne(ArrayLoadSection)

    val outAss = gen.regs.allocate
    val outReg = outAss.getReg()
    
    val accAss = gen.regs.allocate
    val accReg = accAss.getReg()

    val arrayAss = Operands.opToReg(table.getOp(id), gen.regs)

    val finalInstr = Seq(Mov(outReg, accReg))
    
    val instrs = (
      outAss.instr ++ arrayAss.instr ++ accAss.instr ++
      _arrLoad(accReg, arrayAss.getOp(), xs, gen) ++ finalInstr
    )

    return RegAssembly(Register(0), instrs)
  }
  
  def toAssemblyStore(gen: CodeGenerator, rhs: Assembly)(implicit table: Table): RegAssembly = {
    
    gen.postSections.addOne(ArrayBoundsCheck)
    gen.postSections.addOne(ArrayStoreSection)
    
    val arrayAss = Operands.opToReg(table.getOp(id), gen.regs)
    val xsInit = xs.init

    // if array is 1D, only store is required
    if (xsInit.isEmpty) {
      return RegAssembly(
        Register(0),
        arrayAss.instr ++ _arrStore(arrayAss.getOp(), rhs.getOp(), gen)
      )
    }

    // else, use accumulator and include load instructions
    val accAss = gen.regs.allocate
    val accReg = accAss.getReg()
    gen.postSections.addOne(ArrayLoadSection)

    return RegAssembly(
      Register(0),
      arrayAss.instr ++ accAss.instr ++
      _arrLoad(accReg, arrayAss.getOp(), xsInit, gen) ++ 
      _arrStore(accReg, rhs.getOp(), gen)
    )
    
  }
  
  private def _arrStore(array: Operand, value: Operand, gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
    val lastAss = xs.last.toAssembly(gen)
    val func = Func.callFunction("_arrStore", Seq(array, lastAss.getOp(), value), gen)
    return lastAss.instr ++ func
  }

  private def _arrLoad(accum: Register, array: Operand, ys: List[Expr], gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
    val headAss = ys.head.toAssembly(gen)
    val headFunc = Func.callFunction("_arrLoad", Seq(array, headAss.getOp()), gen)
    val accInstr = Seq(Mov(accum, Register(0)))

    val xsFunc = ys.tail.map(x => {
      val xAss = x.toAssembly(gen)
      val xFunc = Func.callFunction("_arrLoad", Seq(accum, xAss.getOp()), gen)
      xAss.instr ++ xFunc ++ accInstr
    }).flatten

    return headAss.instr ++ headFunc ++ accInstr ++ xsFunc
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



