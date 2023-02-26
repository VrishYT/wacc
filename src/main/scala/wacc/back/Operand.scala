package wacc.back

trait Operand

case class ImmInt(i: Int) extends Operand {
    def arm11: String = "#" + i
    override def toString(): String = arm11
}

case class ImmChar(c: Char) extends Operand {
    def arm11: String = "#" + c.toInt
    override def toString(): String = arm11
}

case class ImmLabel(label: String) extends Operand {
    def arm11: String = label
    override def toString(): String = label
}

case class DataLabel(label: String) extends Operand {
    def arm11: String = "=" + label
    override def toString(): String = arm11
}

case class Address(reg: Register, op: Operand) extends Operand{
    def arm11: String = "[" + reg + "," + op + "]"
    override def toString(): String = arm11
}

object Operands {
    def opToReg(op: Operand, regs: RegisterAllocator): RegAssembly = op match {
        case x: Register => RegAssembly(x)
        case _ => {
            val reg = regs.allocate
            RegAssembly(reg.getReg, reg.instr :+ Mov(reg.getReg, op))
        }
    }
}