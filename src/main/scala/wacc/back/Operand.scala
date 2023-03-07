package wacc
package back

trait Operand

case class ImmInt(i: Int) extends Operand {
    def arm11: String = "#" + i
    override def toString(): String = arm11
}

case class ImmChar(c: Char) extends Operand {
    def arm11: String = "#" + c.toInt
    override def toString(): String = arm11
}

case class DataLabel(label: String) extends Operand {
    def arm11: String = "=" + label
    override def toString(): String = arm11
}

case class Address(reg: Register, op: Operand) extends Operand {
    def arm11: String = "[" + reg.toString() + "," + op + "]"
    override def toString(): String = arm11
}

case class ASR(reg: Register, op: Operand) extends Operand {
    def arm11: String = reg.toString() + "," + " asr " + op 
    override def toString(): String = arm11
}

case class LSL(reg: Register, op: Operand) extends Operand {
    def arm11: String = reg.toString() + "," + " lsl " + op 
    override def toString(): String = arm11
}

case class NoOperand(id: String) extends Operand {
    def arm11: String = ""
    override def toString(): String = arm11
}

object Operands {

    def opToReg(op: Operand, dest: Register): Instruction = {
        op match {
            case x: Address => Load(dest, x)
            case x: DataLabel => Load(dest, x)
            case x => Mov(dest, x)
        }
    }

    def opToReg(op: Operand, regs: RegisterAllocator)(implicit table: Table): RegAssembly = op match {
        case x: Register if (x.i != 0) => RegAssembly(x)
        case x => {
            val reg = regs.allocate
            RegAssembly(reg.getReg(), reg.instr :+ opToReg(op, reg.getReg()))
        }
    }

}