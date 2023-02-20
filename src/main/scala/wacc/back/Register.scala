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
    // import scala.collection.mutable.Stack
    import scala.collection.mutable.{Map => MapM}

    // TODO: make private
    val table = MapM[String, Register]() 

    // TODO: check if still needed after re-implement
    val regsInUse = Queue[Register]()
    // val savedRegs = Queue[Register]()

    val freeRegs = Queue(
        Register(0),
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

    def allocate(id: String): RegAssembly = {
        val reg = allocate
        table(id) = reg.getReg
        return reg
    }

    def allocate: RegAssembly = {

        def realloc(): RegAssembly = {
            val reg = regsInUse.head
            val id = table.filter(_._2 == reg).head._1
            return RegAssembly(reg)
        }

        val reg = if (freeRegs.isEmpty) realloc() else RegAssembly(freeRegs.dequeue)
        regsInUse.enqueue(reg.getReg)
        return reg
    }

    def get(id: String): Register = table.get(id) match {
        case Some(x) => x
        case None => ??? // TODO: check if 'id' was pushed to stack/mem and restore to a reg
    }

    // push to stack
    def save(regs: Register*): Instruction = {
        regs.foreach(reg => {
            regsInUse -= reg
            freeRegs.enqueue(reg)
        })
        return Push(regs:_*)
    }

    // pop from stack
    def restore(regs: Register*): Instruction = {
        val reverse = regs.reverse
        regs.foreach(reg => {
            regsInUse.enqueue(reg)
            freeRegs -= reg
        })
        return Pop(regs.reverse:_*)
    }
}