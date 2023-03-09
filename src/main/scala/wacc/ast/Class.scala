package wacc
package ast

import wacc.front.ParserBridge._
import wacc.back._
import wacc._


case class Field(isPrivate: Boolean, t: Type, id: String)(val pos: (Int, Int))

object Field extends ParserBridgePos3[Boolean, Type, String, Field]

case class Class(class_id: String, decls: List[Field], funcs: List[Func])(val pos: (Int, Int)){
    def toAssembly(gen: CodeGenerator): Seq[Instruction] = {
        val func_instr = funcs.map(f => {
            val table = gen.symbolTable.classes.get(class_id) match {
                case Some(x) => x.getMethodTable(f.fs._2) match {
                    case Some(y) => y
                    case None => ???
                }
                case None => ???
            }
            table.resetCounts()
            val out = f.toAssembly(gen, class_id)(table)
            gen.regs.reset()
            out
        })
        return func_instr.fold(Seq())(_ ++ _)
    }
}

object Class extends ParserBridgePos3[String, List[Field], List[Func], Class]

case class NewClass(class_id: String, vals: List[RValue])(val pos: (Int, Int)) extends RValue {

}

object NewClass extends ParserBridgePos2[String, List[RValue], NewClass] 




