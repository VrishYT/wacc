package wacc
package ast

import wacc.back._
import wacc.front.ParserBridge._
import parsley.genericbridges._
import scala.collection.mutable.{ListBuffer}

/* right values as a sealed trait with a position attribute */
trait RValue {
    def pos: (Int, Int)
    def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = TODOAssembly
}

/* case classes for right values */
case class ArrayLiteral(xs: List[Expr])(val pos: (Int, Int)) extends RValue {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {
        val assemblies = xs.map(x => x.toAssembly(gen))
        val instrs = (assemblies.map(x => x.instr)).flatten
        val ops = (assemblies.map(x => x.getOp()))

        val accum = gen.regs.allocate

        val byteType: Boolean = !xs.isEmpty && (xs.head match {
            case _: CharLiteral => true
            case _: BoolLiteral => true
            case _ => false
        })

        val arrAssembly = gen.heap.mallocArray(ops, accum.getReg(), byteType)
        
        return Assembly(
            accum.getReg(),
            instrs ++ accum.instr ++
            (Push(Register(0)) +:
            arrAssembly.instr :+
            Pop(Register(0)))
        )
    }
}

/* companion objects for right values */
object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral]

case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends RValue {
        override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {
        val out = gen.regs.allocate
        val assembly1 = fst.toAssembly(gen)
        val assembly2 = snd.toAssembly(gen)
        val pairAssembly = gen.heap.mallocPair(assembly1.getOp(), assembly2.getOp(), out.getReg())
        return Assembly(out.getReg(), assembly1.instr ++ assembly2.instr ++ out.instr ++ pairAssembly.instr)
    }
}

object NewPair extends ParserBridge2[Expr, Expr, NewPair] {
    def apply(fst: Expr, snd: Expr) = new NewPair(fst, snd)(fst.pos, snd.pos)
}

case class Call(var id: List[String], args: List[Expr])(val pos: (Int, Int)) extends RValue {
    def rename(newId: String): Unit = {
        val newIdList = id.init :+ newId
        id = newIdList
    }

    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {
        
        def callAssembly(ids : List[String], gen: CodeGenerator, instrs: ListBuffer[Instruction], currOp : Operand, classTable: Table) : Assembly = {
            ids match {
                //base case for a function call that is within a class
                case x :: y :: Nil => { 
                    val out = args.map(_.toAssembly(gen))
                    //find the id corresponding to the type of class the variable x represents
                    val classType = classTable.getType(x) match {
                        case Some(x: ClassType) => x.class_id
                        case None => ???
                    } 
                    //feed currOp as the first argument so the reference to the class that holds the function is passed in 
                    //as an argument 
                    val func = Func.callFunction(s"wacc_${classType}_${y}", args = currOp +: out.map(_.getOp()), gen = gen)
                    return Assembly(Register(0), out.map(_.instr).fold(Seq())(_ ++ _) ++ func)
                }

                case classRef :: rest => {
                    //find the id corresponding to the type of class the variable classRef represents
                    val newClassType = classTable.getSymbol(classRef) match {
                        case Some(x) => x.t match {
                            case x: ClassType => x.class_id
                            case _ => ???
                        }
                        case None => ???
                    }
                    //find the corresponding table that represents the variables for the newClassType class
                    val newClassTable = gen.symbolTable.classes.get(newClassType) match {
                        case Some(x) => x
                        case None => ???
                    }

                    def getElemOffset: Int = {
                        val iterator = classTable.table.keysIterator
                        var elemOffset: Option[Int] = None
                        var i = 0
                        while (iterator.hasNext) {
                            if (iterator.next() == classRef) {
                            elemOffset = Some(i*4)
                            } else i += 1
                        } 
                        elemOffset match {
                            case Some(x) => x
                            case None => ??? 
                        }
                    }

                    val elemOffset = getElemOffset
                    //ensure that the correct variable that corresponds to a specific offset is loaded into our temporary variable
                    //the currOp will always hold the memory location of the current class we are dealing with
                    val op: RegAssembly = Operands.opToReg(currOp, gen.regs)
                    instrs ++= (op.instr ++ Seq(Load(Register(12), Address(op.getReg(), ImmInt(elemOffset)))))
                    return callAssembly(ids.tail, gen, instrs, Register(12), newClassTable)
                }
            }
        }

        val list = ListBuffer[Instruction]()
        if (id.length > 1) {
            //if the length is greater than 1 then we want to get the register the class that is 
            //instantiated in stored in, so we use getSymbol
            val classSymbol = table.getSymbol(id.head) match {
                case Some(z : OpSymbol) => z
                case None => ???
            }
            return callAssembly(id, gen, list, classSymbol.op, table)
        } else {
            val out = args.map(_.toAssembly(gen).condToReg(gen.regs))
            // println(s"call ${id.mkString(".")}(${args.mkString(",")})")
            val func = Func.callFunction(s"wacc_${id.head}", args = out.map(_.getOp()), gen = gen)
            return Assembly(Register(0), out.map(_.instr).fold(Seq())(_ ++ _) ++ func)
        }
        
    } 
}

object Call extends ParserBridgePos2[List[String], List[Expr], Call]