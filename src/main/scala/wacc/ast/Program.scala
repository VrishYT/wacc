package wacc
package ast

import scala.collection.mutable.{ListBuffer}
import parsley.genericbridges._
import wacc.back._

/* program case class with its functions and statements */
case class Program(fs: List[Func], stats: List[Stat]) {
    def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): (Seq[Instruction], Seq[Seq[Instruction]]) = {

        val fsOut = fs.map(_.toAssembly(regs, symbolTable))
        val main = stats.map(_.toAssembly(regs, symbolTable)).fold(Seq())(_ ++ _)

        val statsOut = if (!main.isEmpty && main.tail == LinkBranch("exit")) main else main ++ Seq(Mov(Register(0), ImmInt(0)), LinkBranch("exit"))

        return (Seq(Section(".global main"), Label("main")) ++ statsOut, fsOut)
    }
}

/* program companion object with parser bridge */
object Program extends ParserBridge2[List[Func], List[Stat], Program]