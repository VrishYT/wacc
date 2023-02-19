package wacc
package ast

import scala.collection.mutable.{ListBuffer}
import parsley.genericbridges._
import wacc.back._

/* program case class with its functions and statements */
case class Program(fs: List[Func], stats: List[Stat]) {
    def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {

        val instr = ListBuffer[Instruction]()

        // val assFunc = fs.map(_.toAssembly(regs, symbolTable))
        val assStat = stats.foreach(instr ++= _.toAssembly(regs, symbolTable))
        
        // TODO: concat all
        // assStat in 'main'

        return instr.toSeq
    }
}

/* program companion object with parser bridge */
object Program extends ParserBridge2[List[Func], List[Stat], Program]