package wacc
package back

import ast._
import Condition._

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
            val instr = if (cond == NO) Seq() else Seq(Mov(reg.getReg(), ImmInt(1), cond))
            Assembly(reg.getReg(), this.instr ++ reg.instr ++ Seq(Mov(reg.getReg(), ImmInt(0))) ++ instr, cond)
        }
    }
}

/*objects that each ast Node can return with the operand that represents its latest state, 
as well as the instructions, and the latest condition*/
object Assembly {
    def apply(op: Operand, instr: Seq[Instruction], cond: Condition): Assembly = new Assembly(Some(op), instr, cond)
    def apply(op: Operand, instr: Seq[Instruction]): Assembly = apply(op, instr, AL)
    def apply(op: Operand, cond: Condition): Assembly = apply(op, Seq(), cond)
    def apply(op: Operand): Assembly = apply(op, Seq(), AL)
    def apply(instr: Seq[Instruction], cond: Condition): Assembly = new Assembly(None, instr, cond)
}

/*objects that each ast Node can return with the register that represents its latest state, 
as well as the instructions, and the latest condition*/
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
    
    /*presections and post sections are sets that we can add different methods (DataSections) 
    into that will generate all code for printing variables, loading into arrays and reading*/
    val text = new TextSection
    val preSections = scala.collection.mutable.Set[DataSection]()
    val postSections = scala.collection.mutable.Set[DataSection]()

    val labels = new LabelGenerator
    val heap = new HeapAllocator
    val mem = new MemoryAllocator
    val regs = new RegisterAllocator(mem)

    val elemSize = 4
    
    /*returns the entire assembly (DataSection & Main) */
    def toAssembly(program: Program): String = {
        val sb = new StringBuilder
        val out = program.toAssembly(this)

        separateSections(text.toAssembly(), sb)
        preSections.map(x => separateSections(x.toAssembly(), sb))
        separateSections(out._1, sb)
        out._2.map(x => separateSections(x, sb))
        postSections.map(x => separateSections(x.toAssembly(), sb))

        return sb.toString 
    } 
    
    def separateSections(instr: Seq[Instruction], sb: StringBuilder): Unit = {
        instr.foreach(_.arm11(sb))
        sb.append("\n")
    }
}

