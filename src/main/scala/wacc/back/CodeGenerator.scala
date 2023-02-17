package wacc.back

import wacc.AST._

object CodeGenerator {

    def generate(program: Program): Unit = {

        val regs = new RegisterAllocator
                       
        val ass = program.toAssembly(regs)
        ass.foreach(println)
    } 

}