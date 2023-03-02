package wacc
package back

import scala.collection.mutable.ListBuffer

class HeapAllocator {

    val malloc = Seq(
        Push(Register(1), Register(2), Register(3)),
        LinkBranch("malloc"),
        Pop(Register(1), Register(2), Register(3))
    )
    val pop = Pop(Register(8))
    val push = Push(Register(8))
    val elemSize = 4

     def mallocPair(fst: Operand, snd: Operand, out: Register): Assembly = {

        def mallocPairElem(elem: Operand): Seq[Instruction] = {
            val instrs = (Mov(Register(0), ImmInt(elemSize)) +: malloc) ++ Seq(Mov(out, Register(0)),  Mov(Register(8), elem))

            val rest = elem match {
                case ImmChar(_) => Seq(StoreB(Register(8), Address(out, ImmInt(0))), Mov(Register(8), out), push)
                case _ => Seq(Store(Register(8), Address(out, ImmInt(0))),  Mov(Register(8), out), push)
            }
            
            return instrs ++ rest
        }

        val instrs = Seq(push) ++ mallocPairElem(fst) ++ mallocPairElem(snd) ++ Seq(Mov(Register(0), 
                                                                                    ImmInt(elemSize * 2))) ++ 
                                                                                    malloc ++ Seq(
                                                                                     Mov(out, Register(0)),
                                                                                    pop, 
                                                                                    Store(Register(8), Address(out, ImmInt(elemSize))),
                                                                                    pop, 
                                                                                    Store(Register(8), Address(out, ImmInt(0))),
                                                                                    pop
                                                                                    )
        val assembly = Assembly(out, instrs.toSeq)
        return (assembly)
    }

    def mallocArray(xs: List[Operand], out: Register): Assembly = {
        val instrs = ListBuffer[Instruction]()
        val numElems = xs.length

        /* Malloc for the size of array */
        instrs += push
        instrs += Mov(Register(0), ImmInt(elemSize * (numElems + 1)))
        instrs ++= malloc

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
        instrs += pop

        val assembly = Assembly(out, instrs.toSeq)
        return (assembly)
    }
}