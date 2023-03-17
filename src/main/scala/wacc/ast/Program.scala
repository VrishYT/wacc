package wacc
package ast

import parsley.genericbridges._
import wacc.back._

/* program case class with its functions and statements */
case class Program(annotations: List[Annotation], classes : List[Class], fs: List[Func], stats: List[Stat]) {
    def toAssembly(gen: CodeGenerator): (Seq[Instruction], Seq[Seq[Instruction]]) = {

        def getFuncTable(id: String): FuncTable = gen.symbolTable.get(id) match {
            case Some(x) => x
            case None => ???
        }

        //maps classes to assembly 
        val clsOut : Seq[Seq[Instruction]] = classes.map(cls => {
            cls.toAssembly(gen)
        })

        //maps functions into assembly
        val fsOut = fs.map(func => {
            val table = getFuncTable(func.fs._2)
            val out = func.toAssembly(gen)(table)
            gen.regs.reset()
            out
        })
        val mainTable = getFuncTable("_main")
        mainTable.resetCounts()
        val size = 0.max(mainTable.getSize() - gen.regs.freeRegs.size)
        val main = gen.mem.grow(size) +: stats.map(_.toAssembly(gen)(mainTable)).fold(Seq())(_ ++ _) :+ gen.mem.shrink()
        gen.mem.pop()

        return (Seq(Section(".global main"), Label("main"), 
                    Push(FP, LR), Mov(FP, SP)) ++ main ++ Seq(Mov(Register(0), ImmInt(0)), 
                    Pop(FP, PC)), clsOut ++ fsOut)
    }
}

/* program companion object with parser bridge */
object Program extends ParserBridge4[List[Annotation], List[Class], List[Func], List[Stat], Program]