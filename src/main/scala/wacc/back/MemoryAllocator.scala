package wacc.back

class MemoryAllocator {

    import scala.collection.mutable.{Map => MapM}

    val table = MapM[String, Operand]()
    val elemSize = 4

    val malloc = LinkBranch("malloc")

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

        instrs :+ Mov(Register(0), ImmInt(elemSize * (numElems + 1)))
        instrs :+ malloc
        instrs :+ Mov(out, Register(0))
        instrs :+ Add(out, out, ImmInt(elemSize))
        instrs :+ Mov(Register(8), ImmInt(numElems))
        instrs :+ Store(Register(8), Address(out, ImmInt(-elemSize)))

        for (elem <- 1 to numElems) {
            instrs :+ Mov(Register(8), xs(elem - 1))
            instrs :+ Store(Register(8), Address(out, ImmInt((elem - 1) * elemSize)))
        }

        instrs :+ Mov(Register(8), out)

        val assembly = Assembly(out, instrs)
        return (assembly)
    }

    def insert(id: String, add: Operand): Unit = {
        table(id) = add
    }
}