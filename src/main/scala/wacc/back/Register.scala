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
    private val table = MapM[String, Register]() 

    // TODO: check if still needed after re-implement
    private val regsInUse = Queue[Register]()
    // val savedRegs = Queue[Register]()

    private val mem = new MemoryAllocator

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

    def isAllocated(reg: Register): Boolean = regsInUse.contains(reg)

    def link(id: String, reg: Register): Unit = {
        table(id) = reg
    }

    def allocate(id: String): RegAssembly = {
        val reg = allocate
        link(id, reg.getReg)
        return reg
    }

    def allocate: RegAssembly = {

        def realloc(): RegAssembly = {
            val reg = regsInUse.head
            val id = table.filter(_._2 == reg).head._1
            val instr = mem.store(id, reg)
            regsInUse.dequeue
            return RegAssembly(reg, Seq(instr))
        }

        val reg = if (freeRegs.isEmpty) realloc else RegAssembly(freeRegs.dequeue)
        regsInUse.enqueue(reg.getReg)
        return reg
    }

    def get(id: String): Register = table.get(id) match {
        case Some(x) => x
        case None => {
            println(s"Cannot find ${id}")
            println(table)
            ???
        }
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