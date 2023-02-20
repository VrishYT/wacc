package wacc.back

import wacc.ast._
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

case object TODOAssembly extends RegAssembly(None, Seq[Instruction](), AL) 

case class GeneratedAssembly()

object CodeGenerator {

    def generate(program: Program, symbolTable: SymbolTable): String = {

        val regs = new RegisterAllocator

        val out = program.toAssembly(regs, symbolTable)
        val main = out._1.mkString("\n")
        val fs = out._2.map(_.mkString("\n")).fold("\n")(_ + "\n" + _)

        // println(out)
        return symbolTable.data + main + fs
    } 

}