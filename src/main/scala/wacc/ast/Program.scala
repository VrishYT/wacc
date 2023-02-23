package wacc
package ast

import scala.collection.mutable.{ListBuffer}
import parsley.genericbridges._
import wacc.back._

/* program case class with its functions and statements */
case class Program(fs: List[Func], stats: List[Stat]) {
    def toAssembly(regs: RegisterAllocator, mem: MemoryAllocator, symbolTable: SymbolTable): (Seq[Instruction], Seq[Seq[Instruction]]) = {

        val fsOut = fs.map(_.toAssembly(regs, mem, symbolTable))

        val memorySpace = 0 // TODO: calculate memory for main function from symbol table
        mem.reset(memorySpace) // resets memory to be an empty space with this block

        val main = stats.map(_.toAssembly(regs, mem, symbolTable)).fold(Seq())(_ ++ _)

        val statsOut = if (main.tail == LinkBranch("exit")) main else main :+ LinkBranch("exit")

        return (Seq(Section(".global main"), Label("main")) ++ statsOut, fsOut)
    }
}

/* program companion object with parser bridge */
object Program extends ParserBridge2[List[Func], List[Stat], Program]