package wacc
package back

class MemoryAllocator {

    // private val table = MapM[String, Operand]()
    private var size = 0
    // private var count = 0

    // def memorySize(): Int = {
    //     return size
    // }

    def grow(size: Int): Instruction = {
        this.size += size
        Sub(SP, SP, ImmInt(size))
    }

    def shrink(size: Int): Instruction = {
        if (this.size < size) ???
        this.size -= size
        Add(SP, SP, ImmInt(size))
    }

    // def reset(newSize: Int): Unit = {
    //     size = newSize
    //     count = 0
    //     table.clear()
    // }

    // def clear: Unit = {
    //     reset(0)
    // }

    // def setSize(newSize: Int): Unit = {
    //     assert(newSize >= count, "The new size is less than current size, use reset instead.")
    //     size = newSize
    // }

    def store(id: String, reg: Register): Assembly = {
        val operand = Address(SP, ImmInt(-4))
        // count += 1
        // insert(id, operand)
        return Assembly(operand, Seq(Store(reg, operand)))
    }

    // private def insert(id: String, add: Operand): Unit = {
    //     table(id) = add
    // }

    // def get(id: String): Operand = table.get(id) match {
    //     case Some(x) => x
    //     case None => {
    //         println(s"Cannot find ${id}")
    //         println(table)
    //         ???
    //     }
    // }

}