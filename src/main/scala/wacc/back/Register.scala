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

    import scala.collection.mutable.Queue
    import scala.collection.mutable.{ Map => MapM}

    // TODO: make private
    val table = MapM[String, Register]() 

    // TODO: check if still needed after re-implement
    // val regsInUse = Queue[Register]()
    // val savedRegs = Queue[Register]()

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

    def allocate(id: String): Register = {
        val reg = allocate
        table(id) = reg
        return reg
    }

    def allocate: Register = {
        val reg = try {
            freeRegs.dequeue()
        } catch {
            case x: NoSuchElementException => ??? // TODO: free a reg, push reg to stack/mem, update 'table' accordingly
        }
        // regsInUse.enqueue(reg)
        return reg
    }

    def get(id: String): Register = table.get(id) match {
        case Some(x) => x
        case None => ??? // TODO: check if 'id' was pushed to stack/mem and restore to a reg
    }

    // TODO: re-implement
    def save(regs: Register*): Seq[Instruction] = ???
    // def save: Seq[Instruction] = {
    //     val regs = ListBuffer[Register]()
    //     while (!regsInUse.isEmpty) {
    //         val reg = regsInUse.dequeue()
    //         regs += reg
    //         savedRegs.enqueue(reg)
    //     }
    //     val instr = Seq(Push(regs.toSeq:_*))
    //     return instr
    // }

    // TODO: re-implement
    def restore(regs: Register*): Seq[Instruction] = ???
    // def restore: Seq[Instruction] = {
    //     val reverse = savedRegs.reverse
    //     val regs = ListBuffer[Register]()
    //     while (!reverse.isEmpty) {
    //         val reg = reverse.dequeue()
    //         regs += reg
    //         freeRegs.enqueue(reg)
    //     }
    //     val instr = Seq(Pop(regs.toSeq:_*))
    //     return instr
    // }

}