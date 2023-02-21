package wacc.back

class MemoryAllocator(val int: size) {
    import scala.collection.mutable.{Map => MapM}


    // POTENTIAL ARRAY AND PAIR STUFFFFFFF

    var count = 0 // All memory is allocated in blocks of 4B

    val table = MapM[String, Int] // Key: Variable, Value: Address

    def allocate(id: String): Assembly = {
        
        link(id, count)
        val assembly = Assembly(Address(FP, ImmInt((count - size) * 4)))
        return assembly
    }

    def link(id: String, pos: Int): Unit = {
        table(id) = count
    }

    def get(id: String): Operand = table.get(id) match {
        case Some(x) => Assembly(Address(FP, ImmInt((count - size) * 4)))
        case None => {
            println(s"Cannot find ${id}")
            println(table)
            ???
        }
    }
}