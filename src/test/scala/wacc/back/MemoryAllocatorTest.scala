package wacc
package back

import org.scalatest._

class MemoryAllocatorTests extends flatspec.AnyFlatSpec with GivenWhenThen {

  "A new memory allocator" should "have no variables" in {

    val memory = new MemoryAllocator
    val c = memory.getCount
    assert(c == 0)

  }

  "A grow instruction" should "return a valid Instruction object for a stack grow instruction" in {

    val memory = new MemoryAllocator
    memory.size = 7
    val instr = memory.grow()
    assert(instr == Sub(SP, SP, ImmInt(28)))

  }

  "A shrink instruction" should "return a valid Instruction object for a stack shrink instruction" in {

    val memory = new MemoryAllocator
    memory.size = 4
    val instr = memory.shrink()
    assert(instr == Add(SP, SP, ImmInt(16)))

  }


  "A grow instruction" should "return a string for an assembly stack grow instruction" in {

    val memory = new MemoryAllocator
    val sb = new StringBuilder
    memory.size = 2
    val instr = memory.grow().arm11(sb)

    assert(sb.toString == "\tsubs sp, sp, #8\n")

  }

  "A shrink instruction" should "return a string for an assembly stack shrink instruction" in {

    val memory = new MemoryAllocator
    val sb = new StringBuilder
    memory.size = 3
    val instr = memory.shrink().arm11(sb)

    assert(sb.toString == "\tadds sp, sp, #12\n")

  }

  "A memory allocator" should "increase its count by 1 when stored to" in {

    val memory = new MemoryAllocator
    assert(memory.getCount == 0)

    val assembly1 = memory.store("a_variable", Register(4))
    assert(memory.getCount == 1)

    val assembly2 = memory.store("another_variable", Register(5))
    assert(memory.getCount == 2)

  }

  "A memory allocator" should "return a valid Assembly object for a store instruction" in {

    val memory = new MemoryAllocator

    /* Get count before store is called */
    val currCount = memory.getCount

    val reg = Register(1)

    val assembly = memory.store("a_variable", reg)

    /* Identify the instructions and operand in the assembly returned by store */
    val instrs = assembly.instr
    val op = assembly.getOp

    /* Check that store returned the expected Opcode */
    assert(op == Address(FP, ImmInt(-(currCount + 1) * 4)))
    /* Check that there is only one instruction in the sequence returned by store */
    assert(instrs.size == 1)

    /* Retrieve the one instruction returned by store into instr */
    val instr = instrs.head

    /* Check that the instruction returned by store is a Store Instruction from the
       stored register into the frame point -count * 4, (where count is the allocator's
       count before the store). */
    assert(instr == Store(reg, Address(FP, ImmInt(-(currCount + 1) * 4))))

  }

  "A memory allocator" should "return a valid Assembly object for a store instruction after previous stores" in {

    val memory = new MemoryAllocator

    /* Perform 3 stores on different variables */
    memory.store("a_variable1", Register(5))
    memory.store("a_variable2", Register(6))
    memory.store("a_variable3", Register(2))

    /* Get count before next store is called */
    val currCount = memory.getCount

    val reg = Register(7)

    val assembly = memory.store("a_variable", reg)

    /* Identify the instructions and operand in the assembly returned by the store being
       observed */
    val instrs = assembly.instr
    val op = assembly.getOp

    /* Check that store returned the expected Opcode */
    assert(op == Address(FP, ImmInt(-(currCount + 1) * 4)))
    /* Check that there is only one instruction in the sequence returned by store */
    assert(instrs.size == 1)

    /* Retrieve the one instruction returned by store into instr */
    val instr = instrs.head
    /* Check that the instruction returned by the store is a Store Instruction from the
       stored register into the frame point -count * 4, (where count is the allocator's
       count before the store). */
    assert(instr == Store(reg, Address(FP, ImmInt(-(currCount + 1) * 4))))

  }

  "A memory allocator" should "return a valid string for a store instruction" in {

    val memory = new MemoryAllocator

    val reg = Register(1)

    val assembly = memory.store("a_variable", reg)

    /* Identify the instructions and operand in the assembly returned by the store being
       observed */
    val instrs = assembly.instr
    val op = assembly.getOp

    /* Check that the opcode returned by the store provides the expected string for 
       the initial count (0) */
    assert(op.toString == "[fp,#-4]")

    /* Check that there is only one instruction in the sequence returned by store */
    assert(instrs.size == 1)
    /* Retrieve the one instruction returned by store into instr */
    val instr = instrs.head

    /* String build the arm11 string of the instruction */
    val sb = new StringBuilder
    instr.arm11(sb)

    /* Check that the string built arm11 instruction is the expected string for the 
       given register and initial count */
    assert(sb.toString == "\tstr r1, [fp,#-4]\n")
  }

  "A memory allocator" should "return a valid string for a store instruction, after multiple stores" in {

    val memory = new MemoryAllocator

    /* Perform 4 stores on different variables */
    memory.store("a_variable1", Register(5))
    memory.store("a_variable2", Register(6))
    memory.store("a_variable3", Register(2))
    memory.store("a_variable4", Register(6))

    /* Get count before next store is called */
    val currCount = memory.getCount
    /* Calculate the expected ImmInt value of the returned address from the next store */
    val imm = -(currCount + 1) * 4

    val reg = Register(1)

    val assembly = memory.store("a_variable", reg)

    /* Identify the instructions and operand in the assembly returned by the store being
      observed */
    val instrs = assembly.instr
    val op = assembly.getOp

    /* Check the Operand returned by the store has the expected string value */
    assert(op.toString == "[fp,#"+imm+"]")

    /* Check that there is only one instruction in the sequence returned by store */
    assert(instrs.size == 1)
    /* Retrieve the one instruction returned by store into instr */
    val instr = instrs.head

    /* String build the arm11 string of the instruction */
    val sb = new StringBuilder
    instr.arm11(sb)

    /* Check that the string built arm11 instruction is the expected string for the 
       given register and appropriate count (the one recorded just before the store
       being observed by this test) */
    assert(sb.toString == "\tstr r1, [fp,#"+imm+"]\n")

  }
}