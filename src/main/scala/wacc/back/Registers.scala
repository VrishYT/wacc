package wacc.back

sealed class Register(i: Int) extends Operand {
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

class RegisterAllocator {

    import scala.collection.mutable.{Queue, ListBuffer, Map => MapM}

    val table = MapM[String, Either[Int, Register]]()

    val regsInUse = Queue[Register]()
    val savedRegs = Queue[Register]()
    val freeRegs = Queue(
        Register(1), 
        Register(2), 
        Register(3), 
        Register(4), 
        Register(5), 
        Register(6), 
        Register(7), 
        Register(8), 
        Register(9), 
        Register(10), 
        Register(12)
    )

    def allocate: Register = {
        val reg = try {
            freeRegs.dequeue()
        } catch {
            case x: NoSuchElementException => savedRegs.dequeue()
        }
        regsInUse.enqueue(reg)
        return reg
    }

    def save: Seq[Instruction] = {
        val regs = ListBuffer[Register]()
        while (!regsInUse.isEmpty) {
            val reg = regsInUse.dequeue()
            regs += reg
            savedRegs.enqueue(reg)
        }
        val instr = Seq(Push(regs.toSeq:_*))
        return instr
    }

    def restore: Seq[Instruction] = {
        val reverse = savedRegs.reverse
        val regs = ListBuffer[Register]()
        while (!reverse.isEmpty) {
            val reg = reverse.dequeue()
            regs += reg
            freeRegs.enqueue(reg)
        }
        val instr = Seq(Pop(regs.toSeq:_*))
        return instr
    }

}