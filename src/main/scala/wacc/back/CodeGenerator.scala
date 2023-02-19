package wacc.back

import wacc.ast._

object CodeGenerator {

    def generate(program: Program, symbolTable: SymbolTable): Unit = {

        val regs = new RegisterAllocator

        val out = program.toAssembly(regs, symbolTable).mkString("\n")
        println(out)
    } 

}