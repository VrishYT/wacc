package wacc
package back

class RegisterAllocator {

    import scala.collection.mutable.Queue
    // import scala.collection.mutable.Stack
    import scala.collection.mutable.{Map => MapM}

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
        freeRegs.removeFirst(_.i == reg.i)
        regsInUse.enqueue(reg)
    }

    def allocate(id: String): RegAssembly = {
        val reg = allocate
        table(id) = reg.getReg
        return reg
    }

    def allocate: RegAssembly = {


        def realloc(): RegAssembly = {
            val reg = regsInUse.head
            val id = table.filter(_._2 == reg).head._1
            table.remove(id)
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