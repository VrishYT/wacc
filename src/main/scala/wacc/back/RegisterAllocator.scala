package wacc
package back

class RegisterAllocator(val mem: MemoryAllocator) {

    import scala.collection.mutable.Queue
    // import scala.collection.mutable.Stack
    import scala.collection.mutable.{Map => MapM}

    // TODO: REMOVE
    // private val table = MapM[String, Register]() 

    // TODO: check if still needed after re-implement
    private val regsInUse = Queue[Register]()
    // private val varRegs = Queue[Register]() // TODO: keep track of what vars are in regs and only realloc from these if possible

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

    // REMOVE / MOVE ELSWHERE ????
    def link(id: String, reg: Register)(implicit table: Table): Unit = {
        table.update(id, reg)
        freeRegs.removeFirst(_.i == reg.i)
        regsInUse.enqueue(reg)
    }

    def free(reg: Register)(implicit table: Table): Assembly = {
        ??? // TODO
    }

    def allocate(id: String)(implicit table: Table): RegAssembly = {
        val reg = allocate
        table.update(id, reg.getReg())
        println(s"$id -> $reg")
        return reg
    }

    def allocate(implicit table: Table): RegAssembly = {

        // TODO: move and update symbol table with new address
        def realloc(): RegAssembly = {
            val reg = regsInUse.head
            val id = table.getIDFromReg(reg)
            val instr = mem.store(id, reg)
            table.update(id, instr.getOp)
            regsInUse.dequeue
            return RegAssembly(reg, instr.instr)
        }

        val reg = if (freeRegs.isEmpty) realloc() else RegAssembly(freeRegs.dequeue())
        regsInUse.enqueue(reg.getReg())
        println(table)
        println(freeRegs)
        return reg
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