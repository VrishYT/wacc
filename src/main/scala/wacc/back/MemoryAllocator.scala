package wacc.back

class MemoryAllocator {

    import scala.collection.mutable.{Map => MapM}

    val table = MapM[String, Operand]()
    val elemSize = 4

    def mallocPair(fst: Operand, snd: Operand, out: Operand): Assembly = {
        val instrs = Seq()
        instrs :+ Mov(Register(0), ImmInt(elemSize))
        instrs :+ LinkBranch("malloc")

        val assembly = Assembly(Address(FP, ImmInt(0)))
        return (assembly)
    }

    def mallocArray(xs: List[Operand], out: Operand): Assembly = {
        val assembly = Assembly(Address(FP, ImmInt(0)))
        return (assembly)
    }

    def insert(id: String, add: Operand): Unit = {
        table(id) = add
    }
}