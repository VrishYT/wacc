package wacc
package back
import scala.collection.mutable.Stack

class MemoryAllocator {

    private var count = 0
    val stack = Stack[Int]()

    /*grows stack space to store data*/
    def grow(size: Int): Instruction = {
        stack.push(size)
        return Sub(SP, SP, ImmInt(size * 4))
    }

    /*shrinks stack space to store data*/
    def shrink(): Instruction = {
        val size = stack.top
        return Add(SP, SP, ImmInt(size * 4))
    }

    /*pops stack value after use*/
    def pop(): Unit = {
        stack.pop
    }

    /*when register is reallocated, ensures that the data is stored in memory*/
    def store(id: String, reg: Register): Assembly = {
        val operand = Address(FP, ImmInt(-(count + 1) * 4))
        count += 1
        return Assembly(operand, Seq(Store(reg, operand)))
    }

    def getCount(): Int = {
        return count
    }

}