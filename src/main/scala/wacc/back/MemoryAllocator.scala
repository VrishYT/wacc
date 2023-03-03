package wacc
package back

class MemoryAllocator {

    private var count = 0

    def grow(size: Int): Instruction = Sub(SP, SP, ImmInt(size * 4))

    def shrink(size: Int): Instruction = Add(SP, SP, ImmInt(size * 4))

    def store(id: String, reg: Register): Assembly = {
        val operand = Address(FP, ImmInt(-count * 4))
        count += 1
        return Assembly(operand, Seq(Store(reg, operand)))
    }

    def getCount(): Int = {
        return count
    }

}