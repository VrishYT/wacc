package wacc.back

/* for now, memory should include all non-string values */
class MemoryAllocator {
    import scala.collection.mutable.{Map => MapM}

    private var size = 0
    private var count = 0 

    val table = MapM[String, Int]() // Key: Variable, Value: Pos

    def reset(newSize: Int): Unit = {
        size = newSize
        count = 0
        table.clear()
    }

    def allocate(id: String): Assembly = {
        link(id, count)
        val assembly = Assembly(Address(FP, ImmInt((count - size) * 4)))
        return assembly
    }

    def link(id: String, pos: Int): Unit = {
        table(id) = count
    }

    def get(id: String): Operand = table.get(id) match {
        case Some(x) => Address(FP, ImmInt((count - size) * 4))
        case None => {
            println(s"Cannot find ${id}")
            println(table)
            ???
        }
    }
}