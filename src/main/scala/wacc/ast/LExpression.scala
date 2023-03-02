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
  def getInnerType(t: Type, xs: Seq[Expr]): Type = {
    if (xs.isEmpty) t
    else t match {
      case ArrayType(t) => getInnerType(t, xs.tail)
      case _ => ???
    }
  }

  private def checkCharType()(implicit table: Table): Boolean = {
    val arrayType = table.getType(id) match {
      case Some(x) => getInnerType(x, xs)
      case None => ???
    }

    return (arrayType == CharType)
  }

  override def toAssembly(gen: CodeGenerator)(implicit table: Table): RegAssembly = {
    return toAssemblyLoad(gen)
  }

  def toAssemblyLoad(gen: CodeGenerator)(implicit table: Table): RegAssembly = {

    gen.postSections.addOne(ArrayBoundsCheck)
    gen.postSections.addOne(PrintStringSection)

    val charType = checkCharType()
    if (charType) {
      gen.postSections.addOne(ArrayLoadBSection)
    } else {
      gen.postSections.addOne(ArrayLoadSection)
    }
    

    val outAss = gen.regs.allocate
    val outReg = outAss.getReg()
    
    val accAss = gen.regs.allocate
    val accReg = accAss.getReg()
    gen.regs.free(accReg)

    val arrayAss = Operands.opToReg(table.getOp(id), gen.regs)

    val finalInstr = Seq(Mov(outReg, accReg))
    
    val instrs = (
      outAss.instr ++ arrayAss.instr ++ accAss.instr ++
      _arrLoad(accReg, arrayAss.getOp(), xs, gen, charType) ++ finalInstr
    )

    return RegAssembly(accReg, instrs)
  }
  
  def toAssemblyStore(gen: CodeGenerator, rhs: Assembly)(implicit table: Table): RegAssembly = {
    
    gen.postSections.addOne(ArrayBoundsCheck)
    gen.postSections.addOne(PrintStringSection)
    val charType = checkCharType()
    if (charType) {
      gen.postSections.addOne(ArrayStoreBSection)
    } else {
      gen.postSections.addOne(ArrayStoreSection)
    }
    
    val arrayAss = Operands.opToReg(table.getOp(id), gen.regs)
    val xsInit = xs.init

    // if array is 1D, only store is required
    if (xsInit.isEmpty) {
      return RegAssembly(
        Register(0),
        arrayAss.instr ++ _arrStore(arrayAss.getOp(), rhs.getOp(), gen, charType)
      )
    }

    // else, use accumulator and include load instructions
    val accAss = gen.regs.allocate
    val accReg = accAss.getReg()
    gen.regs.free(accAss.getReg)
    if (charType) {
      gen.postSections.addOne(ArrayLoadBSection)
    } else {
    gen.postSections.addOne(ArrayLoadSection)
    }

    return RegAssembly(
      Register(0),
      arrayAss.instr ++ accAss.instr ++
      _arrLoad(accReg, arrayAss.getOp(), xsInit, gen, charType) ++ 
      _arrStore(accReg, rhs.getOp(), gen, charType)
    )
    
  }
  
  private def _arrStore(array: Operand, value: Operand, gen: CodeGenerator, charType: Boolean)(implicit table: Table): Seq[Instruction] = {
    val lastAss = xs.last.toAssembly(gen)
    val label = if (charType) "_arrStoreB" else "_arrStore"
    val func = Func.callFunction(label, Seq(array, lastAss.getOp(), value), gen)
    return lastAss.instr ++ func
  }

  private def _arrLoad(accum: Register, array: Operand, ys: List[Expr], gen: CodeGenerator, charType: Boolean)(implicit table: Table): Seq[Instruction] = {
    val headAss = ys.head.toAssembly(gen)
    val label = if (charType) "_arrLoadB" else "_arrLoad"
    val headFunc = Func.callFunction(label, Seq(array, headAss.getOp()), gen)
    val accInstr = Seq(Mov(accum, Register(0)))

    val xsFunc = ys.tail.map(x => {
      val xAss = x.toAssembly(gen)
      val xFunc = Func.callFunction(label, Seq(accum, xAss.getOp()), gen)
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



