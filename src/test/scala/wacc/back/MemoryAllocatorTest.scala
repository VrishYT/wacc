package wacc.back

import org.scalatest._

class MemoryAllocatorTests extends flatspec.AnyFlatSpec with GivenWhenThen {

  "A new memory allocator" should "have no variables" in {
    val memory = new MemoryAllocator
    assert(memory.memorySize == 0)
  }

  it should "be able to change in size" in {
    val memory = new MemoryAllocator
    memory.setSize(10)
    assert(memory.memorySize == 10)
  }

  it should "allow variables to be stored" in {
    Given("a new memory allocator of size 1, a register and a variable stored in that register")
    val mem = new MemoryAllocator
    mem.setSize(1)
    val reg = Register(0)
    val id = "default"

    When("a variable is stored from a register")
    val instr = mem.store(id, reg)

    Then("the memory allocator should have size 1")
    assert(mem.memorySize === 1)

    And("it should return an instruction to move the value from the register into memory")
    assert(instr == Store(reg, Address(FP, ImmInt(0))))

    And("we can query it for the new memory location of the variable")
    val addr = mem.get(id)
    assert(addr == Address(FP, ImmInt(0)))
  }
}