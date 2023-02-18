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

case class DataLabel(label: String) extends Operand {
    def arm11: String = "=" + label
    override def toString(): String = arm11
}

object Operands {
    def opToReg(op: Operand, regs: RegisterAllocator): (Register, Seq[Instruction]) = op match {
        case x: Register => (x, Seq())
        case _ => {
            val reg = regs.allocate
            (reg, Seq(Mov(reg, op)))
        }
    }
}