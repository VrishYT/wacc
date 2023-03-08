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
    

     def mallocPair(fst: Operand, snd: Operand, out: Register): Assembly = {

        val elemSize = 4
        /*malloc pairs and malloc elements inside pair*/
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

    def mallocArray(xs: List[Operand], accum: Register, byte: Boolean): Assembly = {
        
        val instrs = ListBuffer[Instruction]()
        val numElems = xs.length

        val elemSize = if (byte) 1 else 4
        val temp = Register(0)

        /* Malloc for the size of array */
        instrs += Mov(Register(0), ImmInt(elemSize * (numElems + 1)))
        instrs ++= malloc

        /* Set accum to malloced address + elemSize */
        instrs += Mov(accum, Register(0))
        instrs += Add(accum, accum, ImmInt(elemSize))

        /* Store array length at malloced memory location */
        instrs += Mov(temp, ImmInt(numElems))
        instrs += Store(temp, Address(accum, ImmInt(-elemSize)))

        /* Store array elems in correct mem location */
        for (elem <- 1 to numElems) {
            val index = elem - 1
            instrs += Operands.opToReg(xs(index), temp)
            instrs += Store(temp, Address(accum, ImmInt(index * elemSize)))
        }

        val assembly = Assembly(accum, instrs.toSeq)
        return (assembly)
    }
}