package wacc.back

class MemoryAllocator {

    import scala.collection.mutable.{Map => MapM}

    val table = MapM[String, Operand]()
    var size = 0
    var count = 0

    def reset(newSize: Int): Unit = {
        size = newSize
        count = 0
        table.clear()
    }

    def store(id: String, reg: Register): Instruction = {
        val operand = Address(FP, ImmInt(count * 4))
        count += 1
        insert(id, operand)

        return Store(reg, operand)
    }

    def insert(id: String, add: Operand): Unit = {
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