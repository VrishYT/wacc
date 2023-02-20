package wacc
package ast

import scala.collection.mutable.{ListBuffer}
import parsley.genericbridges._
import wacc.back._

/* program case class with its functions and statements */
case class Program(fs: List[Func], stats: List[Stat]) {
    def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {

        val fsOut = /* fs.map(_.toAssembly(regs, symbolTable)).fold(Seq())(_ ++ _) */ Seq() // TODO
        val statsOut = stats.map(_.toAssembly(regs, symbolTable)).fold(Seq())(_ ++ _)

        return Seq(Section(".global main"), Label("main")) ++ fsOut ++ statsOut
    }
}

/* program companion object with parser bridge */
object Program extends ParserBridge2[List[Func], List[Stat], Program]