package wacc
package ast

import scala.collection.mutable.{ListBuffer}
import parsley.genericbridges._
import wacc.back._

/* program case class with its functions and statements */
case class Program(fs: List[Func], stats: List[Stat]) {
    def toAssembly(gen: CodeGenerator): (Seq[Instruction], Seq[Seq[Instruction]]) = {

        def getFuncTable(id: String): FuncTable = gen.symbolTable.get(id) match {
            case Some(x) => x
            case None => ???
        }

        val fsOut = fs.map(func => func.toAssembly(gen, getFuncTable(func.fs._2)))
        val main = stats.map(_.toAssembly(gen, getFuncTable("main"))).fold(Seq())(_ ++ _)

        val statsOut = if (!main.isEmpty && main.tail == LinkBranch("exit")) main else main ++ Seq(Mov(Register(0), ImmInt(0)), LinkBranch("exit"))

        return (Seq(Section(".global main"), Label("main")) ++ statsOut, fsOut)
    }
}

/* program companion object with parser bridge */
object Program extends ParserBridge2[List[Func], List[Stat], Program]