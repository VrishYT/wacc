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

}