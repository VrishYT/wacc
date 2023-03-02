package wacc
package ast

import parsley.genericbridges._
import wacc.back._

/* program case class with its functions and statements */
case class Program(fs: List[Func], stats: List[Stat]) {
    def toAssembly(gen: CodeGenerator): (Seq[Instruction], Seq[Seq[Instruction]]) = {

        def getFuncTable(id: String): FuncTable = gen.symbolTable.get(id) match {
            case Some(x) => x
            case None => ???
        }

        val fsOut = fs.map(func => func.toAssembly(gen)(getFuncTable(func.fs._2)))
        val main = stats.map(_.toAssembly(gen)(getFuncTable("main"))).fold(Seq())(_ ++ _)

        return (Seq(Section(".global main"), Label("main"), Push(FP, LR), Mov(FP, SP)) ++ main ++ Seq(Mov(Register(0), ImmInt(0)), Pop(FP, PC)), fsOut)
    }
}

/* program companion object with parser bridge */
object Program extends ParserBridge2[List[Func], List[Stat], Program]