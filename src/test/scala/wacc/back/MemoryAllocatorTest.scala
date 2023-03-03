package wacc
package back

import org.scalatest._

//@Ignore
class MemoryAllocatorTests extends flatspec.AnyFlatSpec with GivenWhenThen {

  "A new memory allocator" should "have no variables" in {
    val memory = new MemoryAllocator
    val c = memory.getCount
    assert(c == 0)
  }

  "A grow instruction" should "return a valid Instruction object for a stack grow instruction" in {
    val memory = new MemoryAllocator
    val instr = memory.grow(7)
    assert(instr == Sub(SP, SP, ImmInt(28)))
  }

  "A shrink instruction" should "return a valid Instruction object for a stack shrink instruction" in {
    val memory = new MemoryAllocator
    val instr = memory.shrink(4)
    assert(instr == Add(SP, SP, ImmInt(16)))
  }


  "A grow instruction" should "return a string for an assembly stack grow instruction" in {
    val memory = new MemoryAllocator
    val sb = new StringBuilder
    val instr = memory.grow(2).arm11(sb)
    assert(sb.toString == "\tsubs sp, sp, #8\n")
  }

  "A shrink instruction" should "return a string for an assembly stack shrink instruction" in {
    val memory = new MemoryAllocator
    val sb = new StringBuilder
    val instr = memory.shrink(3).arm11(sb)
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

    val currCount = memory.getCount
    val reg = Register(1)

    val assembly = memory.store("a_variable", reg)

    val instrs = assembly.instr
    val op = assembly.getOp

    assert(op == Address(FP, ImmInt(-currCount * 4)))
    assert(instrs.size == 1)

    val instr = instrs.head
    assert(instr == Store(reg, Address(FP, ImmInt(-currCount * 4))))
  }

  "A memory allocator" should "return a valid Assembly object for a store instruction after previous stores" in {
    val memory = new MemoryAllocator

    memory.store("a_variable1", Register(5))
    memory.store("a_variable2", Register(6))
    memory.store("a_variable3", Register(2))

    val currCount = memory.getCount

    val reg = Register(7)

    val assembly = memory.store("a_variable", reg)

    val instrs = assembly.instr
    val op = assembly.getOp

    assert(op == Address(FP, ImmInt(-currCount * 4)))
    assert(instrs.size == 1)

    val instr = instrs.head
    assert(instr == Store(reg, Address(FP, ImmInt(-currCount * 4))))
  }

  "A memory allocator" should "return a valid string for a store instruction" in {
    val memory = new MemoryAllocator

    val reg = Register(1)

    val assembly = memory.store("a_variable", reg)

    val instrs = assembly.instr
    val op = assembly.getOp

    assert(op.toString == "[fp,#0]")

    assert(instrs.size == 1)
    val instr = instrs.head

    val sb = new StringBuilder
    instr.arm11(sb)

    assert(sb.toString == "\tstr r1, [fp,#0]\n")
  }

  "A memory allocator" should "return a valid string for a store instruction, after multiple stores" in {
    val memory = new MemoryAllocator

    memory.store("a_variable1", Register(5))
    memory.store("a_variable2", Register(6))
    memory.store("a_variable3", Register(2))
    memory.store("a_variable4", Register(6))

    val currCount = memory.getCount
    val imm = -currCount * 4

    val reg = Register(1)

    val assembly = memory.store("a_variable", reg)

    val instrs = assembly.instr
    val op = assembly.getOp

    assert(op.toString == "[fp,#"+imm+"]")

    assert(instrs.size == 1)
    val instr = instrs.head

    val sb = new StringBuilder
    instr.arm11(sb)

    assert(sb.toString == "\tstr r1, [fp,#"+imm+"]\n")
  }
}