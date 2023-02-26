package wacc.back

class MemoryAllocator {

    import scala.collection.mutable.{Map => MapM}

    val table = MapM[String, Address]()

    def mallocPair(fst: Operand, snd: Operand): Assembly = {
        val assembly = Assembly(Address(FP, ImmInt(0)))
        return (assembly)
    }
}