package wacc
package back

class RegisterAllocator(val mem: MemoryAllocator) {

    import scala.collection.mutable.Queue

    private val regsInUse = Queue[Register]()
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
        Register(10)
    )

    def reset(): Unit = {
        while (!regsInUse.isEmpty) {
            val reg = regsInUse.dequeue()
            freeRegs.enqueue(reg)
        }
    }

    def isAllocated(reg: Register): Boolean = regsInUse.contains(reg)

    def link(id: String, reg: Register)(implicit table: Table): Unit = {
        table.update(id, reg)
        freeRegs.removeFirst(_.i == reg.i)
        regsInUse.enqueue(reg)
    }

    def free(reg: Register): Unit = {
        if (regsInUse.contains(reg) && !freeRegs.contains(reg)) {
            regsInUse -= reg
            freeRegs += reg
        }
    }

    def allocate(id: String)(implicit table: Table): RegAssembly = {
        val reg = allocate
        table.update(id, reg.getReg())
        return reg
    }

    def allocate(implicit table: Table): RegAssembly = {

        def realloc(): RegAssembly = {
            val reg = regsInUse.dequeue()
            table.getIDFromReg(reg) match {
                case Some(x) => {
                    val instr = mem.store(x, reg)
                    table.update(x, instr.getOp())
                    return RegAssembly(reg, instr.instr)
                }
                case None => {
                    regsInUse.enqueue(reg)
                    realloc()
                }
            }
        }

        val reg = if (freeRegs.isEmpty) realloc() else RegAssembly(freeRegs.dequeue())
        regsInUse.enqueue(reg.getReg())
        return reg
    }

    def save(regs: Register*): Instruction = {
        regs.foreach(reg => {
            regsInUse -= reg
            freeRegs.enqueue(reg)
        })
        return Push(regs:_*)
    }

    def restore(regs: Register*): Instruction = {
        regs.foreach(reg => {
            regsInUse.enqueue(reg)
            freeRegs -= reg
        })
        return Pop(regs:_*)
    }
}