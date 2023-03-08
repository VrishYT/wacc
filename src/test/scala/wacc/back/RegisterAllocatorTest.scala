package wacc
package back
package ast

import org.scalatest._

class RegisterAllocatorTests extends flatspec.AnyFlatSpec with GivenWhenThen {

  "A new register allocator" should "have no allocated registers for r1 to r10" in {

    val memory = new MemoryAllocator
    val regAllocator = new RegisterAllocator(memory)

    for (i <- 1 to 10) {
        assert(!regAllocator.isAllocated(Register(i)))
    }
  }

  "A register allocator" should "be able to save a list of registers" in {

    val memory = new MemoryAllocator
    val regAllocator = new RegisterAllocator(memory)

    val reg1 = Register(1)
    val reg2 = Register(2)
    val reg3 = Register(3)

    val instr = regAllocator.save(reg1, reg2, reg3)

    assert(instr == Push(reg1, reg2, reg3))
  }

  "A register allocator" should "be able to restore a list of registers" in {

    val memory = new MemoryAllocator
    val regAllocator = new RegisterAllocator(memory)

    val reg1 = Register(6)
    val reg2 = Register(7)
    val reg3 = Register(8)
    val reg4 = Register(10)

    val instr = regAllocator.save(reg1, reg2, reg3, reg4)

    assert(instr == Push(reg1, reg2, reg3, reg4))
  }
/*
  "A register allocator" should "should be able to allocate a register" in {

    val memory = new MemoryAllocator
    val regAllocator = new RegisterAllocator(memory)
    val table = new SymbolTable

    val init_size = regAllocator.freeRegs.size

    val regAssemb = regAllocator.allocate(table)

    assert(regAllocator.freeRegs.size == init_size - 1)
  }
*/
}