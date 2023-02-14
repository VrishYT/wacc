package wacc.back

sealed trait Register extends Operand

/* Stack Pointer */ 
case object SP extends Register {
    override def toString(): String = "sp"
}

/* Linked Register */
case object LR extends Register {
    override def toString(): String = "lr"
}

/* Program Counter */
case object PC extends Register {
    override def toString(): String = "pc"
}

/* Frame Pointer */
case object FP extends Register {
    override def toString(): String = "fp"
}

/* General Purpose Registers */
case class Reg(i: Int) extends Register {
    override def toString(): String = "r" + i
}