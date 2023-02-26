package wacc.back

class MemoryAllocator {

    import scala.collection.mutable.{Map => MapM}

    val table = MapM[String, Operand]()
    val elemSize = 4

    def mallocPair(fst: Operand, snd: Operand, out: Register): Assembly = {
        def mallocElem(elem: Operand): Seq[Instruction] = {
            val instrs = Seq()
            instrs :+ Mov(Register(0), ImmInt(elemSize))
            instrs :+ LinkBranch("malloc")
            instrs :+ Mov(out, Register(0))
            instrs :+ Mov(Register(8), elem)
            instrs :+ Store(Register(8), Address(out, ImmInt(0)))
            instrs :+ Mov(Register(8), out)
            instrs :+ Push(Register(8))
            return instrs
        }
        val instrs = mallocElem(fst) ++ mallocElem(snd)

        instrs :+ Mov(Register(0), ImmInt(elemSize * 2))
        instrs :+ LinkBranch("malloc")
        instrs :+ Mov(out, Register(0))
        instrs :+ Pop(Register(8))
        instrs :+ Store(Register(8), Address(out, ImmInt(elemSize)))
        instrs :+ Pop(Register(8))
        instrs :+ Store(Register(8), Address(out, ImmInt(0)))
        instrs :+ Mov(Register(8), out)

        val assembly = Assembly(Register(8), instrs)
        return (assembly)
    }

    def mallocArray(xs: List[Operand], out: Register): Assembly = {
        val assembly = Assembly(Address(FP, ImmInt(0)))
        return (assembly)
    }

    def insert(id: String, add: Operand): Unit = {
        table(id) = add
    }
}