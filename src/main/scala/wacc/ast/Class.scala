package wacc
package ast

import front.ParserBridge._
import back._
import scala.collection.mutable.{ListBuffer}
import wacc.front.error.{ErrorLogger}

case class Field(isPrivate: Boolean, t: Type, id: String)(val pos: (Int, Int))

object Field extends ParserBridgePos3[Boolean, Type, String, Field]

case class Class(annotations: List[Annotation], class_id: String, decls: List[Field], funcs: List[Func])(val pos: (Int, Int)){
    def toAssembly(gen: CodeGenerator): Seq[Instruction] = {
        if (decls.isEmpty && funcs.isEmpty){
            ErrorLogger.warn(s"${class_id} is an empty class", pos._1)
        }
        val func_instr = funcs.map(f => {
            val table: MethodTable = gen.symbolTable.classes.get(class_id) match {
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

object Class extends ParserBridgePos4[List[Annotation], String, List[Field], List[Func], Class]

case class NewClass(class_id: String, vals: List[RValue])(val pos: (Int, Int)) extends RValue {
    override def toAssembly(gen: CodeGenerator)(implicit table : Table): Assembly = {
        val classTable = gen.symbolTable.classes.get(class_id) match {
            case Some(x) => x
            case None => ???
        }
        val constructor_types = classTable.types
        val class_size = (constructor_types.length + 1) * 4
        val out = gen.regs.allocate
        val instrs = Mov(Register(0), ImmInt(class_size)) +: (HeapAllocator.malloc ++ out.instr ++ 
                    Seq(
                        Mov(out.getReg(), Register(0)),
                        Add(out.getReg(), out.getReg(), ImmInt(4)),
                        Mov(Register(0), ImmInt(class_size)),
                        Store(Register(0), Address(out.getReg(), ImmInt(-4)))
                        ))
        val list = ListBuffer[Seq[Instruction]]()
        for (i <- 0 to vals.length -1){
            val offset = i * 4 
            val rvalInstr = vals(i).toAssembly(gen)(table)
            val totalInstr = rvalInstr.instr ++ loadRval(constructor_types(i), vals(i), offset, out.getReg(), rvalInstr.getOp())
            list += totalInstr
        }
        
        val instrs2 = list.fold(Seq())(_ ++ _)
        gen.regs.free(out.getReg())
        return Assembly(out.getReg(), instrs ++ instrs2)
    }

    def loadRval(t : Type, rval: RValue, offset: Int, out: Register, elemReg: Operand) : Seq[Instruction] = {
        val elemSize = if (t == CharType || t == BoolType) 1 else 4
        val loadMov = if (t == StringType) Seq(Load(Register(12), elemReg)) else Seq(Mov(Register(12), elemReg))
        val instrs = (Mov(Register(0), ImmInt(elemSize)) +: HeapAllocator.malloc) ++ loadMov
        val rest = elemSize match {
            case 1 => Seq(StoreB(Register(12), Address(Register(0), ImmInt(0))))
            case _ => Seq(Store(Register(12), Address(Register(0), ImmInt(0))))
        }
        val finalInstr = Seq(Store(Register(0), Address(out, ImmInt(offset))))
        return instrs ++ rest ++ finalInstr
    }
}

object NewClass extends ParserBridgePos2[String, List[RValue], NewClass] 




