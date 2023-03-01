package wacc
package back

class HeapAllocator {


    import scala.collection.mutable.{Map => MapM, ListBuffer}

    val malloc = LinkBranch("malloc")
    val pop = Pop(Register(8))
    val push = Push(Register(8))
    val elemSize = 4

    def mallocPair(fst: Operand, snd: Operand, out: Register): Assembly = {

        def mallocPairElem(elem: Operand): ListBuffer[Instruction] = {
            val instrs = ListBuffer[Instruction]()

            /* Malloc with elemSize parameter */
            instrs += Mov(Register(0), ImmInt(elemSize))
            instrs += malloc

            /* Move allocated mem address into out */
            instrs += Mov(out, Register(0))

            /* Move elem into allocated address */
            instrs += Mov(Register(8), elem)
            instrs += Store(Register(8), Address(out, ImmInt(0)))

            /* Push address of elem */
            instrs += Mov(Register(8), out)
            instrs += push
            
            return instrs
        }

        val instrs = mallocPairElem(fst) ++= mallocPairElem(snd)
        push +=: instrs

        /* Malloc with pair size parameter */
        instrs += Mov(Register(0), ImmInt(elemSize * 2))
        instrs += malloc

        /* Move allocated mem address into out */
        instrs += Mov(out, Register(0))

        /* Pop and store snd elem address in pair address + 4 */
        instrs += pop
        instrs += Store(Register(8), Address(out, ImmInt(elemSize)))

         /* Pop and store fst elem address in pair address */
        instrs += pop
        instrs += Store(Register(8), Address(out, ImmInt(0)))
        instrs += pop

        val assembly = Assembly(out, instrs.toSeq)
        return (assembly)
    }

    def mallocArray(xs: List[Operand], out: Register): Assembly = {
        val instrs = ListBuffer[Instruction]()
        val numElems = xs.length

        /* Malloc for the size of array */
        instrs += push
        instrs += Mov(Register(0), ImmInt(elemSize * (numElems + 1)))
        instrs += malloc

        /* Set out to malloced address + elemSize */
        instrs += Mov(out, Register(0))
        instrs += Add(out, out, ImmInt(elemSize))

        /* Store array length at malloced memory location */
        instrs += Mov(Register(8), ImmInt(numElems))
        instrs += Store(Register(8), Address(out, ImmInt(-elemSize)))

        /* Store array elems in correct mem location */
        for (elem <- 1 to numElems) {
            instrs += Mov(Register(8), xs(elem - 1))
            instrs += Store(Register(8), Address(out, ImmInt((elem - 1) * elemSize)))
        }

        /* Store address of first element in out */
        instrs += Mov(out, Register(8))
        instrs += pop

        val assembly = Assembly(out, instrs.toSeq)
        return (assembly)
    }
}