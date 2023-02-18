package wacc.back

import wacc.AST._

object CodeGenerator {

    def generate(program: Program, symbolTable: SymbolTable): Unit = {

        val regs = new RegisterAllocator
                       
        val out = program.toAssembly(regs, symbolTable)mkString("\n")
        println(out)
    } 

}