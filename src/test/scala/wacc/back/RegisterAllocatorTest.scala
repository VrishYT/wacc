package wacc
package back
package ast

import org.scalatest._

class RegisterAllocatorTests extends flatspec.AnyFlatSpec with GivenWhenThen {

    val fs = (IntType, "getX")
    val args = List()
    val stats = List(Declare(IntType, "x", IntLiteral(9), Return(Ident("x"))))
    val modelAst = Program(List(Func(fs, args, stats)),  List(Skip))

  "A new register allocator" should "have no allocated registers for r1 to r10" in {

    val memory = new MemoryAllocator
    val regAllocator = new RegisterAllocator(memory)

    for (i <- 1 to 10) {
        assert(!regAllocator.isAllocated(Register(i)))
    }
  }


/*
  "A new register allocator" should "have no allocated registers for r1 to r10" in {

    val memory = new MemoryAllocator
    val regAllocator = new RegisterAllocator(memory)
    val symbolTable = new SymbolTable

    def getFuncTable(id: String): FuncTable = symbolTable.get(id) match {
            case Some(x) => x
            case None => ???
     }

    val init_size = regAllocator.freeRegs.size

    val regAssemb = regAllocator.allocate

    assert(regAllocator.freeRegs.size == init_size - 1)
  }
  */
}