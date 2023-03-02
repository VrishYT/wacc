package wacc
package back

import ast._
import Condition._
import scala.collection.mutable.{Set => SetM}

class Assembly(val op: Option[Operand], val instr: Seq[Instruction], var cond: Condition) {
    def getOp(): Operand = op match {
        case Some(x) => x
        case None => ???
    }
    def not(): Assembly = {
        cond = Condition.invert(cond)
        this
    }
    def condToReg(regs: RegisterAllocator)(implicit table: Table): Assembly = op match {
        case Some(x) => this
        case None => {
            val reg = regs.allocate
            Assembly(reg.getReg, this.instr ++ reg.instr ++ Seq(Mov(reg.getReg, ImmInt(0)), Mov(reg.getReg, ImmInt(1), cond)), cond)
        }
    }
}

object Assembly {
    def apply(op: Operand, instr: Seq[Instruction], cond: Condition): Assembly = new Assembly(Some(op), instr, cond)
    def apply(op: Operand, instr: Seq[Instruction]): Assembly = apply(op, instr, AL)
    def apply(op: Operand, cond: Condition): Assembly = apply(op, Seq(), cond)
    def apply(op: Operand): Assembly = apply(op, Seq(), AL)
    def apply(instr: Seq[Instruction], cond: Condition): Assembly = new Assembly(None, instr, cond)
}

class RegAssembly(val reg: Option[Register], instr: Seq[Instruction], cond: Condition) extends Assembly(reg, instr, cond) {
    def getReg(): Register = reg match {
        case Some(x) => x
        case None => ???
    }
}

object RegAssembly {
    def apply(reg: Register, instr: Seq[Instruction], cond: Condition): RegAssembly = new RegAssembly(Some(reg), instr, cond)
    def apply(reg: Register, instr: Seq[Instruction]): RegAssembly = apply(reg, instr, AL)
    def apply(reg: Register, cond: Condition): RegAssembly = apply(reg, Seq(), cond)
    def apply(reg: Register): RegAssembly = apply(reg, Seq(), AL)
    def apply(instr: Seq[Instruction], cond: Condition): RegAssembly = new RegAssembly(None, instr, cond)
}

object TODOAssembly extends RegAssembly(None, Seq[Instruction](), AL)

case class CodeGenerator(val symbolTable: SymbolTable) {
    
    val text = new TextSection
    val preSections = scala.collection.mutable.Set[DataSection]()
    val postSections = scala.collection.mutable.Set[DataSection]()

    val labels = new LabelGenerator
    val heap = new HeapAllocator
    val mem = new MemoryAllocator
    val regs = new RegisterAllocator(mem)

    val elemSize = 4

    def toAssembly(program: Program): String = {
        val sb = new StringBuilder
        val out = program.toAssembly(this)
        val pre = (preSections.addOne(text)).map(x => separateSections(x.toAssembly, sb))
        
        val main = out._1.foreach(_.arm11(sb))
        sb.append("\n")
        val fs = out._2.map(x => separateSections(x, sb))
        val post = postSections.map(x => separateSections(x.toAssembly, sb))

        // println(out)
        return sb.toString 
        //pre + main + fs + post 
    } 
    
    def separateSections(instr: Seq[Instruction], sb: StringBuilder){
        instr.foreach(_.arm11(sb))
        sb.append("\n")
    }
}

