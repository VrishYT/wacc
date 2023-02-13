package wacc.back

import wacc.AST._

object CodeGenerator {

    def generate(program: Program): Unit = {
        val regs = Seq(Reg(1), 
                       Reg(2), 
                       Reg(3), 
                       Reg(4),
                       Reg(5),
                       Reg(6),
                       Reg(7),
                       Reg(8),
                       Reg(9),
                       Reg(10),
                       Reg(11),
                       Reg(12))
                       
        val ass = program.toAssembly(regs)
        println(ass)
    } 

}