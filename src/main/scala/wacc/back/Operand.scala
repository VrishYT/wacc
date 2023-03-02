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
    def arm11: String = "[" + reg + "," + op + "]"
    override def toString(): String = arm11
}

case class ASR(reg: Register, op: Operand) extends Operand {
    def arm11: String = reg + "," + " asr " + op 
    override def toString(): String = arm11
}

object Operands {

    def opToReg(op: Operand, dest: Register): Instruction = op match {
        // TODO: modify for new operands (ASR, Address)
        case x: DataLabel => Load(dest, x)
        case x => Mov(dest, x)
    }

    def opToReg(op: Operand, regs: RegisterAllocator)(implicit table: Table): RegAssembly = op match {
        case x: Register => RegAssembly(x)
        case x => {
            val reg = regs.allocate
            val instr: Instruction = x match {
                case x: Address => Load(reg.getReg, x)
                case x: DataLabel => Load(reg.getReg, x)
                case _ => Mov(reg.getReg, x)
            }
            RegAssembly(reg.getReg, reg.instr :+ instr)
        }
    }

}