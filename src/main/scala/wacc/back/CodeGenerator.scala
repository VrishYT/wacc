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

object CodeGenerator {

    def generate(program: Program, symbolTable: SymbolTable): Unit = {

        val regs = new RegisterAllocator

        val out = program.toAssembly(regs, symbolTable).mkString("\n")
        println(out)
    } 

}