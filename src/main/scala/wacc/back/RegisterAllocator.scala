package wacc
package back

class RegisterAllocator(val mem: MemoryAllocator) {

    import scala.collection.mutable.Queue

    // TODO: REMOVE
    // private val table = MapM[String, Register]() 

    // TODO: check if still needed after re-implement
    private val regsInUse = Queue[Register]()
    // private val varRegs = Queue[Register]() // TODO: keep track of what vars are in regs and only realloc from these if possible

    val freeRegs = Queue(
        // Register(0),
        Register(1), 
        Register(2), 
        Register(3), 
        Register(4), 
        Register(5), 
        Register(6), 
        Register(7), 
        Register(8), 
        Register(9), 
        Register(10)/*, 
        Register(12)*/
    )

    def reset(): Unit = {
        // println(s"inUse: $regsInUse")
        // regsInUse.foreach(r => {
        //     println(r)
        //     free(r)
        // })
        while (!regsInUse.isEmpty) {
            val reg = regsInUse.dequeue()
            freeRegs.enqueue(reg)
        }
    }

    def isAllocated(reg: Register): Boolean = regsInUse.contains(reg)

    // REMOVE / MOVE ELSWHERE ????
    def link(id: String, reg: Register)(implicit table: Table): Unit = {
        table.update(id, reg)
        freeRegs.removeFirst(_.i == reg.i)
        regsInUse.enqueue(reg)
    }

    def free(reg: Register): Unit = {
        if (regsInUse.contains(reg) && !freeRegs.contains(reg)) {
            // println(s"free: $freeRegs")
            // println(s"inUse: $regsInUse")
            regsInUse -= reg
            freeRegs += reg
            // println(s"freed $reg")
        }
    }

    def allocate(id: String)(implicit table: Table): RegAssembly = {
        val reg = allocate
        table.update(id, reg.getReg())
        // println(s"$id -> $reg")
        return reg
    }

    def allocate(implicit table: Table): RegAssembly = {

        // TODO: move and update symbol table with new address
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
        // println(table)
        // println(freeRegs)
        // println(s"alloc ${reg.getReg()}")
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
        regs.foreach(reg => {
            regsInUse.enqueue(reg)
            freeRegs -= reg
        })
        return Pop(regs:_*)
    }
}