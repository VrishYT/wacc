package wacc.back

class MemoryAllocator {

    import scala.collection.mutable.{Map => MapM}

    val table = MapM[String, Operand]()

    def mallocPair(fst: Operand, snd: Operand): Assembly = {
        val assembly = Assembly(Address(FP, ImmInt(0)))
        return (assembly)
    }

    def insert(id: String, add: Operand): Unit = {
        table(id) = add
    }
}