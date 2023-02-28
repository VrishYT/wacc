package wacc.back

class MemoryAllocator {

    import scala.collection.mutable.{Map => MapM}

    private val table = MapM[String, Operand]()
    private var size = 0
    private var count = 0

    def memorySize(): Int = {
        return size
    }

    def reset(newSize: Int): Unit = {
        size = newSize
        count = 0
        table.clear()
    }

    def clear: Unit = {
        reset(0)
    }

    def setSize(newSize: Int): Unit = {
        assert(newSize >= count, "The new size is less than current size, use reset instead.")
        size = newSize
    }

    def store(id: String, reg: Register): Instruction = {
        val operand = Address(FP, ImmInt(-(count + 1) * 4))
        count += 1
        insert(id, operand)

        return Store(reg, operand)
    }

    private def insert(id: String, add: Operand): Unit = {
        table(id) = add
    }

    def get(id: String): Operand = table.get(id) match {
        case Some(x) => x
        case None => {
            println(s"Cannot find ${id}")
            println(table)
            ???
        }
    }


    val malloc = LinkBranch("malloc")
    val elemSize = 4

    def mallocPair(fst: Operand, snd: Operand, out: Register): Assembly = {

        val pop = Pop(Register(8))
        val push = Push(Register(8))

        def mallocElem(elem: Operand): Seq[Instruction] = {
            val instrs = Seq()

            /* Malloc with elemSize parameter */
            instrs :+ Mov(Register(0), ImmInt(elemSize))
            instrs :+ malloc

            /* Move allocated mem address into out */
            instrs :+ Mov(out, Register(0))

            /* Move elem into allocated address */
            instrs :+ Mov(Register(8), elem)
            instrs :+ Store(Register(8), Address(out, ImmInt(0)))

            /* Push address of elem */
            instrs :+ Mov(Register(8), out)
            instrs :+ push

            return instrs
        }

        val instrs = mallocElem(fst) ++ mallocElem(snd)

        /* Malloc with pair size parameter */
        instrs :+ Mov(Register(0), ImmInt(elemSize * 2))
        instrs :+ malloc

        /* Move allocated mem address into out */
        instrs :+ Mov(out, Register(0))

        /* Pop and store snd elem address in pair address + 4 */
        instrs :+ pop
        instrs :+ Store(Register(8), Address(out, ImmInt(elemSize)))

         /* Pop and store fst elem address in pair address */
        instrs :+ pop
        instrs :+ Store(Register(8), Address(out, ImmInt(0)))

        val assembly = Assembly(out, instrs)
        return (assembly)
    }

    def mallocArray(xs: List[Operand], out: Register): Assembly = {
        val instrs = Seq()
        val numElems = xs.length

        /* Malloc for the size of array */
        instrs :+ Mov(Register(0), ImmInt(elemSize * (numElems + 1)))
        instrs :+ malloc

        /* Set out to malloced address + elemSize */
        instrs :+ Mov(out, Register(0))
        instrs :+ Add(out, out, ImmInt(elemSize))

        /* Store array length at malloced memory location */
        instrs :+ Mov(Register(8), ImmInt(numElems))
        instrs :+ Store(Register(8), Address(out, ImmInt(-elemSize)))

        /* Store array elems in correct mem location */
        for (elem <- 1 to numElems) {
            instrs :+ Mov(Register(8), xs(elem - 1))
            instrs :+ Store(Register(8), Address(out, ImmInt((elem - 1) * elemSize)))
        }

        /* Store address of first element in out */
        instrs :+ Mov(out, Register(8))

        val assembly = Assembly(out, instrs)
        return (assembly)
    }
}