package wacc
package ast

import wacc.front.ParserBridge._
import wacc.back._
import scala.collection.mutable.ListBuffer

/* left expressions extending expressions and left values */
sealed trait LExpr extends Expr with LValue {
    def valid: Boolean = true
    def pos: (Int, Int)
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = TODOAssembly
}

/* expressions extending left expressions */
case class Ident(id: String)(val pos: (Int, Int)) extends LExpr {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {
      // println(id)
      table.getOp(id) match {
        case x@NoOperand(id) => {
          if (table.containsRecursive("this")) ClassElem(List("this", id))(pos).toAssembly(gen)
          else Assembly(x)
        }
        case _ => Assembly(table.getOp(id))
      } 
    }
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
    
    val outReg = Register(0)
    val accReg = Register(8)

    val arrayAss = Operands.opToReg(table.getOp(id), gen.regs)

    val finalInstr = Seq(Mov(outReg, accReg))
    
    val instrs = (
      arrayAss.instr ++ Seq(Push(Register(8))) ++
      _arrLoad(accReg, arrayAss.getOp(), xs, gen, charType) ++ finalInstr ++ Seq(Pop(Register(8)))
    )

    return RegAssembly(outReg, instrs)
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
    val accReg = Register(8)
    if (charType) {
      gen.postSections.addOne(ArrayLoadBSection)
    } else {
      gen.postSections.addOne(ArrayLoadSection)
    }

    val helpAss = gen.regs.allocate
    
    val reg = RegAssembly(
      Register(0),
      arrayAss.instr ++ helpAss.instr ++ Seq(Push(Register(8)), Mov(helpAss.getReg(), rhs.getOp())) ++
      _arrLoad(accReg, arrayAss.getOp(), xsInit, gen, charType) ++ 
      _arrStore(accReg, helpAss.getReg(), gen, charType) ++ Seq(Pop(Register(8)))
    )
    
    gen.regs.free(helpAss.getReg())

    return reg
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
  
// case class IdentOrArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr

/* identifier or array elements with parser bridge */
object LExpr extends ParserBridgePos2[List[String], Option[List[Expr]], LExpr] {
    def apply(ids: List[String], xs: Option[List[Expr]])(pos: (Int, Int)): LExpr = xs match {
        case None => ids match {
          case id :: Nil => Ident(id)(pos)
          case _ => ClassElem(ids)(pos)
        }
        case Some(xs) => ids match {
          case id :: Nil => ArrayElem(id, xs)(pos)
          case _ => InvalidLExpr(pos)
        }
    }
}


case class ClassElem(ids: List[String])(val pos: (Int, Int)) extends LExpr {
  override def toAssembly(gen: CodeGenerator)(implicit table: Table): RegAssembly = {
    
    val classAss = Operands.opToReg(table.getOp(ids.head), Register(12))
    val classType = table.getType(ids.head) match {
        case Some(x : ClassType) => x.class_id
        case None => ???
    }
    val instrs = ListBuffer[Instruction]()
    instrs += classAss
    val load = toAssemblyLoad(gen, ids.tail, Register(12), classType, instrs)(table)
    RegAssembly(load.getReg(), Seq(Comment(s"class elem ${ids.mkString(".")}"), classAss) ++ load.instr)
  }

  def toAssemblyLoad(gen: CodeGenerator, ids: List[String], accumReg: Register, classType: String, instrs: ListBuffer[Instruction], address: Boolean = false)(implicit table: Table): RegAssembly = {
    
    def getElemOffset(classTable : Table, id: String): Int = {
      val iterator = classTable.table.keysIterator
      var elemOffset: Option[Int] = None
      var i = 0
      while (iterator.hasNext) {
        if (iterator.next() == id) {
          elemOffset = Some(i*4)
        } else i += 1
      } 
      elemOffset match {
        case Some(x) => x
        case None => ??? 
      }
    }
    
    ids match {
      case y :: Nil => {
        val outReg = Register(0)
        val classTable = gen.symbolTable.classes.get(classType) match {
          case Some(a) => a
          case None => ???
        }
        val elemType = classTable.getSymbol(y) match {
          case Some(z) => z.t
          case None => ???
        }

        val elemOffset = getElemOffset(classTable, y)

        val loadByte = (elemType == CharType || elemType == BoolType)
        if (address) {
          return RegAssembly(outReg, instrs.toSeq ++ loadAddrClassElem(accumReg, outReg, elemOffset))
        }else {
          return RegAssembly(outReg, instrs.toSeq ++ loadClassElem(accumReg, outReg, elemOffset, loadByte))
        }
      }

      case _ => {
        val newClassTable = gen.symbolTable.classes.get(classType) match {
          case Some(a) => a
          case None => ???
        }
        val newClassType = newClassTable.getType(ids.head) match {
          case Some(x) => x match {
            case y : ClassType => y.class_id
            case _ => ???
          }
          case None => ???
        }

        val elemOffset = getElemOffset(newClassTable, ids.head)
        val temp = gen.regs.allocate
        instrs ++= Seq(
                  Load(temp.getReg, Address(accumReg, ImmInt(elemOffset))), 
                  Load(accumReg, Address(temp.getReg, ImmInt(0))))
        return toAssemblyLoad(gen, ids.tail, accumReg, newClassType, instrs)
      }
    }
  }

  def loadAddrClassElem(reg: Register, outReg: Register, offset: Int): Seq[Instruction] = {
      return Seq(Load(outReg, Address(reg, ImmInt(offset))))
  }

  def loadClassElem(reg: Register, outReg: Register, offset: Int, byte : Boolean): Seq[Instruction] = {
      return Seq(Load(outReg, Address(reg, ImmInt(offset))),
                 Load(outReg, Address(outReg, ImmInt(0)), byte))
  }
}

object ClassElem extends ParserBridgePos1[List[String], ClassElem] 

case class InvalidLExpr(val position: (Int, Int)) extends LExpr {
  override def valid: Boolean = false
  override def pos: (Int, Int) = position
}


