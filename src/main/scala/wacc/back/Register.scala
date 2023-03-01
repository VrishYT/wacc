package wacc
package back

sealed class Register(val i: Int) extends Operand {
    override def toString(): String = "r" + i
}

object Register {
    def apply(i: Int): Register = i match {
        case x if (x >= 0 && x < 11) => new Register(x)
        case x if (x == 12) => new Register(x)
        case _ => ???
    }
}

/* Stack Pointer */ 
case object SP extends Register(13) {
    override def toString(): String = "sp"
}

/* Linked Register */
case object LR extends Register(14) {
    override def toString(): String = "lr"
}

/* Program Counter */
case object PC extends Register(15) {
    override def toString(): String = "pc"
}

/* Frame Pointer */
case object FP extends Register(11) {
    override def toString(): String = "fp"
}