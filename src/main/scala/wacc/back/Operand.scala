package wacc
package back

trait Operand

/*operand to represent immediate integers*/
case class ImmInt(i: Int) extends Operand {
    def arm11: String = "#" + i
    override def toString(): String = arm11
}

/*operand to represent immediate characters*/
case class ImmChar(c: Char) extends Operand {
    def arm11: String = "#" + c.toInt
    override def toString(): String = arm11
}

/*operand to represent addressing of a label*/
case class DataLabel(label: String) extends Operand {
    def arm11: String = "=" + label
    override def toString(): String = arm11
}

/*operand to represent memory address inside register, second argument 
to shift along in memory*/
case class Address(reg: Register, op: Operand) extends Operand {
    def arm11: String = "[" + reg.toString() + "," + op + "]"
    override def toString(): String = arm11
}

/*arithmetic shift right*/
case class ASR(reg: Register, op: Operand) extends Operand {
    def arm11: String = reg.toString() + "," + " asr " + op 
    override def toString(): String = arm11
}

/*logical shift left*/
case class LSL(reg: Register, op: Operand) extends Operand {
    def arm11: String = reg.toString() + "," + " lsl " + op 
    override def toString(): String = arm11
}

object Operands {
    /*assigns a specific register to our operand*/
    def opToReg(op: Operand, dest: Register): Instruction = {
        op match {
            case x: Address => Load(dest, x)
            case x: DataLabel => Load(dest, x)
            case x => Mov(dest, x)
        }
    }

    /*finds a register to allocate our operand to*/
    def opToReg(op: Operand, regs: RegisterAllocator)(implicit table: Table): RegAssembly = op match {
        case x: Register if (regs.isAllocated(x)) => RegAssembly(x)
        case x: Register if (x.i != 0) => {
            println(s"use $x")
            regs.use(x)
            RegAssembly(x)
        }
        case x => {
            val reg = regs.allocate
            RegAssembly(reg.getReg(), reg.instr :+ opToReg(op, reg.getReg()))
        }
    }

}