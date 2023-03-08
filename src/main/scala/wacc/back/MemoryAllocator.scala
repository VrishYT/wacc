package wacc
package back

class MemoryAllocator {

    private var count = 0
    var size = 0

    /*grows stack space to store data*/
    def grow(): Instruction = Sub(SP, SP, ImmInt(size * 4))

    /*shrinks stack space to store data*/
    def shrink(): Instruction = {
        val instr = Add(SP, SP, ImmInt(size * 4))
        size = 0
        instr
    }

    /*when register is reallocated, ensures that the data is stored in memory*/
    def store(id: String, reg: Register): Assembly = {
        val operand = Address(FP, ImmInt(-count * 4))
        count += 1
        return Assembly(operand, Seq(Store(reg, operand)))
    }

    def getCount(): Int = {
        return count
    }

}