package wacc
package ast

import wacc.back._
import parsley.genericbridges._ 
import front.ParserBridge._
import scala.collection.mutable.{ListBuffer}


/* statements as objects extending the sealed trait Stat */
sealed trait Stat {
    def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction]
}

case object Skip extends Stat with ParserBridge0[Stat] {
    def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = Seq()
}

case class Break(val pos: (Int, Int)) extends Stat with ParserBridgePos0[Stat] {
    def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = table.getWhileLabels match {
        case Some(x) => Seq(Branch(x._2))
        case _ => ???
    }
}

case class Continue(val pos: (Int, Int)) extends Stat with ParserBridgePos0[Stat] {
    def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = table.getWhileLabels match {
        case Some(x) => Seq(Branch(x._1))
        case _ => ???
    }
}

case class Declare(annotations: List[Annotation], t: Type, id: String, rhs: RValue) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        val assembly = rhs.toAssembly(gen).condToReg(gen.regs)
        val instr = rhs match {
            case x: Ident => {
                val reg = gen.regs.allocate(id)
                reg.instr :+ Mov(reg.getReg(), assembly.getOp())
            }
            case _ => {
                val reg = rhs match {
                    case _: ArrayElem => {
                        val out = gen.regs.allocate
                        RegAssembly(
                            out.getReg(),
                            out.instr ++ Seq(Mov(out.getReg(), assembly.getOp()))
                        )
                    }
                    case _ => Operands.opToReg(assembly.getOp(), gen.regs)
                    }
                table.update(id, reg.getReg())
                reg.instr
            }
        }
        return Comment(s"declare $id") +: (assembly.instr ++ instr)
    }
}

object Declare extends ParserBridge4[List[Annotation], Type, String, RValue, Declare]

case class AssignOrTypelessDeclare(annotations: List[Annotation], x: LValue, y: RValue) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        val rhsAssembly = y.toAssembly(gen).condToReg(gen.regs)
        val lval = x match {
            case arrElem@ArrayElem(_, _) => {
                val reg = y match {
                    case _: ArrayElem => {
                        val out = gen.regs.allocate
                        Assembly(out.getReg(), rhsAssembly.instr ++ Seq(Mov(out.getReg(), rhsAssembly.getOp())))
                    }
                    case _ => rhsAssembly
                }
                return reg.instr ++ arrElem.toAssemblyStore(gen, reg).instr
            }
            case x: PairElem => {
                val addrAssemb = x.getAddr(gen)
                val reg = Operands.opToReg(rhsAssembly.getOp(), gen.regs)
                return (addrAssemb.instr ++ reg.instr ++ rhsAssembly.instr ++ 
                        Seq(Store(reg.getReg(), addrAssemb.getOp())))
            }
            case x: ClassElem => {
                def toAssemblyStore(): RegAssembly = {
                    val classAss = Operands.opToReg(table.getOp(x.ids.head), Register(12))
                    val classType = table.getType(x.ids.head) match {
                        case Some(x : ClassType) => x.class_id
                        case _ => ???
                    }
                    val instrs = ListBuffer[Instruction]()
                    instrs += classAss
                    val load = x.toAssemblyLoad(gen, x.ids.tail, Register(12), classType, instrs, true)(table)
                    RegAssembly(load.getReg(), Seq(Comment(s"class elem ${x.ids.mkString(".")}"), classAss) ++ load.instr)
                }
                val clsElemAss = toAssemblyStore()
                val reg = Operands.opToScratch(rhsAssembly.getOp())
                val instr = y match {
                    case CharLiteral(_) | BoolLiteral(_) => Seq(Store(reg.getReg(), clsElemAss.getReg(), false, true)) 
                    case _ => Seq(Store(reg.getReg(), Address(clsElemAss.getReg()), false, false))
                }
                return (clsElemAss.instr ++ reg.instr ++ rhsAssembly.instr ++ instr)
            } 

            case _ => x.toAssembly(gen)
        }
        
        lval.getOp() match {
            case NoOperand(id) => {
                val opt = table.getType(id)
                val t = opt match {
                    case Some(x) => x
                    case None => ???
                }
                return Declare(annotations, t, id, y).toAssembly(gen)
            }
            case _ =>
        }

        y match {
            case _: ArrayElem => {
                val out = gen.regs.allocate
                val reg = Assembly(
                    out.getReg(),
                    out.instr ++ rhsAssembly.instr ++ Seq(Mov(out.getReg(), rhsAssembly.getOp()))
                )
                return AssignOrTypelessDeclare.assDec(lval, reg, gen.regs)
            }
            case StrLiteral(string) => {
                return AssignOrTypelessDeclare.assDec(lval, rhsAssembly, gen.regs)
            }
            case _ => {
                return AssignOrTypelessDeclare.assDec(lval, rhsAssembly, gen.regs)
            }
        }
    }
}

object AssignOrTypelessDeclare extends ParserBridge3[List[Annotation], LValue, RValue, AssignOrTypelessDeclare] {
    def assDec(lval: Assembly, rval: Assembly, regs: RegisterAllocator)(implicit table: Table) : Seq[Instruction] = {
        val reg = Operands.opToReg(rval.getOp(), regs)
        val save = lval.getOp() match {
            case x: Register => Seq(Mov(x, reg.getOp()))
            case x: Address => {
                val temp = Operands.opToReg(reg.getOp(), regs)
                regs.free(temp.getReg())
                temp.instr :+ Store(temp.getReg(), x)
            }
        }
        return (rval.instr ++ lval.instr ++ reg.instr) ++ save
    }
}

case class Read(x: LValue) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        x match {
            /* reading in identifiers requires finding the type of the variable 
            and then executing specific instructions for it */
            case id@Ident(i) => {
                val identType = table.getType(i) match {
                    case Some(x) => x
                    case None => ???
                }

                val ass = id.toAssembly(gen)
                identType match {
                    case IntType => {
                        gen.postSections.addOne(ReadIntSection)
                        val reg = Operands.opToReg(ass.getOp(), gen.regs)
                        return ass.instr ++ reg.instr ++ readInt(reg.getReg())
                    } 
                    case CharType => 
                        gen.postSections.addOne(ReadCharSection)
                        val reg = Operands.opToReg(ass.getOp(), gen.regs)
                        return ass.instr ++ reg.instr ++ readChar(reg.getReg())
                    case _ => Seq()
                }
            }
            /*reading in pairs requires figuring out the type of the first or second values and 
            executing the appropriate code */
            case p@Fst(x) => {
                val ass = p.toAssembly(gen)
                x match {
                    case id@Ident(i) => {
                        val identType = table.getType(i) match {
                            case Some(x) => x
                            case None => ???
                        }
                        identType match {
                            case PairType(IntType, _) => {
                                gen.postSections.addOne(ReadIntSection)
                                return ass.instr ++ readInt(ass.getReg())
                            }
                            case PairType(CharType, _) => {
                                gen.postSections.addOne(ReadCharSection)
                                return ass.instr ++ readChar(ass.getReg())
                            }
                            case _ => ???
                        }
                    }
                }
            }
            case p@Snd(x) => {
                val ass = p.toAssembly(gen)
                x match {
                    case id@Ident(i) => {
                        val identType = table.getType(i) match {
                            case Some(x) => x
                            case None => ???
                        }
                        identType match {
                            case PairType(_, IntType) => {
                                gen.postSections.addOne(ReadIntSection)
                                return ass.instr ++ readInt(ass.getReg())
                            }
                            case PairType(_, CharType) => {
                                gen.postSections.addOne(ReadCharSection)
                                return ass.instr ++ readChar(ass.getReg())
                            }
                            case _ => ???
                        }
                    }
                }

            }
        }
    }

    def readInt(reg: Register): Seq[Instruction] = {
        Seq(
            Push(Register(12)),
            Push(Register(0), Register(1)),
            Mov(Register(0), reg),
            LinkBranch("_readi"),
            Mov(Register(12), Register(0)),
            Pop(Register(0), Register(1)),
            Mov(reg, Register(12)),
            Pop(Register(12))
        )
    }

    def readChar(reg: Register): Seq[Instruction] = {
        Seq(
            Push(Register(12)),
            Push(Register(0), Register(1)),
            Mov(Register(0), reg),
            LinkBranch("_readc"),
            Mov(Register(12), Register(0)),
            Pop(Register(0), Register(1)),
            Mov(reg, Register(12)),
            Pop(Register(12))
        )
    }
}

object Read extends ParserBridge1[LValue, Read]

case class Free(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        val xAssembly = x.toAssembly(gen)

        /*freeing only for arrays and pairs*/
        x match {
            case Ident(id) => {
                table.getType(id) match {
                    case Some(x) => x match {
                        case _: ArrayType => freeArray(xAssembly, gen)
                        case _: PairType => freePair(xAssembly, gen)
                        case Pair => freePair(xAssembly, gen)
                        case _ => ???
                    }
                    case None => ???
                }
            }
            case _: ArrayElem => freeArray(xAssembly, gen)
            case _ => ???
        }
    }

    def freeArray(assembly: Assembly, gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {

        val regAssembly = Operands.opToReg(assembly.getOp(), gen.regs)
        /*register 0 is used by the C function free therefore we need to save it */
        return assembly.instr ++ regAssembly.instr ++ Seq(
            Push(Register(0)),
            Sub(Register(0), regAssembly.getReg(), ImmInt(4)),
            LinkBranch("free"),
            Pop(Register(0))
        )
    }

    def freePair(assembly: Assembly, gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        gen.postSections.addOne(NullDereference)
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(FreePairSection)

        val xOp = assembly.getOp()
        /*register 8 is clobbered as we load in the pair's value 
        into it therefore we need to push it*/
        val instrns = assembly.instr ++ Seq(Push(Register(8)), Mov(Register(8), xOp), 
                                         Mov(Register(0), Register(8)), LinkBranch("_freepair"), 
                                         Mov(Register(0), ImmInt(0)), Pop(Register(8)))
        return instrns
    }
}


object Free extends ParserBridge1[Expr, Free]

case class Return(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        /*we use the register 0 as the return register, and remove it off 
        the register list so that it is not used for storing other values*/
        val expr = x.toAssembly(gen).condToReg(gen.regs)
        if (expr.getOp() == Register(0)) return expr.instr 
        else return expr.instr ++ 
            Seq(
                Mov(Register(0), expr.getOp()), 
                gen.mem.shrink(), 
                Pop((Func.FuncRegs :+ PC):_*)
            )
    }
}

object Return extends ParserBridge1[Expr, Return]

case class Exit(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        /*register 0 is used by C function exit*/
        val ass = x.toAssembly(gen)
        ass.instr ++ Seq(
                Mov(Register(0), ass.getOp()),
                LinkBranch("exit"))
    }
}

object Exit extends ParserBridge1[Expr, Exit]

case class Print(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        /*we need to case match on the expression so that we can execute appropriate code for 
        printing different types such as ints, strings, characters, booleans. */
        Comment("start print") +: (
        x match {
            /*need to case match on identifier to check what is the type of the value 
            assigned to identifier and print accordingly*/
            case id@Ident(i) => {
                val identType = table.getType(i) match {
                    case Some(x) => x
                    case None => ???
                }
                val ass = id.toAssembly(gen)
                val reg = Operands.opToScratch(ass.getOp())
                ass.instr ++ reg.instr ++ printValue(identType, reg.getReg(), gen)
            }
            case int: IntLiteral => {
                val ass = int.toAssembly(gen)
                ass.instr ++ printValue(IntType, ass.getOp(), gen)
            }
            case str: StrLiteral => {
                val ass = str.toAssembly(gen)
                val reg = gen.regs.allocate 
                gen.regs.free(reg.getReg())
                ass.instr ++ reg.instr ++ (Load(reg.getReg(), ass.getOp()) +: printValue(StringType, reg.getReg(), gen))
            }
            case char: CharLiteral => {
                val ass = char.toAssembly(gen)
                ass.instr ++ printValue(CharType, ass.getOp(), gen)
            }
            case bool: BoolLiteral => {
                val ass = bool.toAssembly(gen)
                ass.instr ++ printValue(BoolType, ass.getOp(), gen)
            }
            /*need to case match on unary operators to check what is the type of the evaluated 
            expression*/
            case unop@UnaryOpExpr(op, _) => {
                val unopType = op.output 
                val ass = unop.toAssembly(gen).condToReg(gen.regs)
                ass.instr ++ printValue(unopType, ass.getOp(), gen)
            }
            /*need to case match on binary operators to check what is the type of 
            the evaluated expression*/
            case binop@BinaryOpExpr(op, _, _) => {
                val binopType = op.output 
                val ass = binop.toAssembly(gen).condToReg(gen.regs)
                ass.instr ++ printValue(binopType, ass.getOp(), gen)
            }
            /*need to case match on array elems to check the type and print accordingly*/
            case a@ArrayElem(id, xs) => { 

                def getType(t: Type, xs: Seq[Expr]): Type = {
                    if (xs.isEmpty) t
                    else t match {
                        case ArrayType(t) => getType(t, xs.tail)
                        case _ => ???
                    }
                }
                gen.postSections.addOne(PrintStringSection) 
                val identType = table.getType(id) match {
                    case Some(x) => getType(x, xs)
                    case None => ???
                }
                val ass = a.toAssembly(gen)
                ass.instr ++ printValue(identType, ass.getOp(), gen)
            }

            case c@ClassElem(ids) => {
                def findType(ids: List[String], currTable: Table) : Type = {
                    ids match {
                        case y :: Nil => {
                            val classType = currTable.getType(y) match {
                                case Some(z) => z 
                                case None => ???
                            }
                            return classType
                        }
                        case id :: rest => {
                            val classType = currTable.getType(id) match {
                                case Some(x) => x match {
                                    case ClassType(id) => id
                                    case _ => ???
                                }
                                case None => ???
                            }
                            val classTable = gen.symbolTable.classes.get(classType) match {
                                case Some(x) => x
                                case None => ???
                            }
                            return findType(rest, classTable)
                        }
                        case _ => ???
                    }
                }
                
                val classType = table.getType(ids.head) match {
                    case Some(x) => {
                        x match {
                            case ClassType(id) => {
                                gen.symbolTable.classes.get(id) match {
                                    case Some(y) => findType(ids.tail, y)
                                    case None => ???
                                }
                            }
                            case _ => ???
                        }
                    }
                    case None => ???
                }
                val ass = c.toAssembly(gen)
                ass.instr ++ printValue(classType, ass.getOp(), gen)
            }
            /*need to case match to print pair nulls accordingly*/
            case p@PairLiteralNull(_) => {
                gen.postSections.addOne(PrintPointerSection)
                val ass = p.toAssembly(gen)
                ass.instr ++ Seq(
                    Push(Register(0), Register(1), Register(2), Register(3)),
                    Mov(Register(1), ass.getOp()),
                    LinkBranch("_printp"),
                    Pop(Register(0), Register(1), Register(2), Register(3))
                )
            }
            case _ => Seq()
        }) :+ Comment("end print") 
    }



    def printValue(baseType: Type, operand: Operand, gen: CodeGenerator) : Seq[Instruction] = {
        baseType match {
            /*we push register 0,1,2,3 before prints to avoid clobbering*/
            case StringType => {
                gen.postSections.addOne(PrintStringSection) 
                return Seq(
                    Push(Register(0), Register(1), Register(2), Register(3)),
                    Mov(Register(2), operand),
                    Load(Register(1), Address(Register(2), ImmInt(-4))),
                    LinkBranch("_prints"),
                    Pop(Register(0), Register(1), Register(2), Register(3))
                )
            }
            case IntType => {
                gen.postSections.addOne(PrintIntSection)
                return Seq(
                    Push(Register(0), Register(1), Register(2), Register(3)),
                    Mov(Register(1), operand),
                    LinkBranch("_printi"),
                    Pop(Register(0), Register(1), Register(2), Register(3))
                )
            }
            case CharType => {
                gen.postSections.addOne(PrintCharSection)
                return Seq(
                    Push(Register(0), Register(1), Register(2), Register(3)),
                    Mov(Register(1), operand),
                    LinkBranch("_printc"),
                    Pop(Register(0), Register(1), Register(2), Register(3))
                )
            }
            case BoolType => {
                gen.postSections.addOne(PrintBoolSection)
                gen.postSections.addOne(PrintStringSection) 
                return Seq(
                    Push(Register(0), Register(1), Register(2), Register(3)),
                    Mov(Register(0), operand),
                    LinkBranch("_printb"),
                    Pop(Register(0), Register(1), Register(2), Register(3))
                )
            }

            case ArrayType(CharType) => {
                gen.postSections.addOne(PrintStringSection) 
                return Seq(
                    Push(Register(0), Register(1), Register(2), Register(3)),
                    Mov(Register(2), operand),
                    Load(Register(1), Address(Register(2), ImmInt(-4))),
                    LinkBranch("_prints"),
                    Pop(Register(0), Register(1), Register(2), Register(3))
                )
            }
            
            case PairType(_,_) | ArrayType(_) | ClassType(_) => {
                gen.postSections.addOne(PrintPointerSection)
                return Seq(
                    Push(Register(0), Register(1), Register(2), Register(3)),
                    Mov(Register(1), operand),
                    LinkBranch("_printp"),
                    Pop(Register(0), Register(1), Register(2), Register(3))
                )
            }
            case _ => return Seq()
        }
    }
}

object Print extends ParserBridge1[Expr, Print]

case class Println(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        gen.postSections.addOne(PrintNewLine)
        (Comment("start println") +: Print(x).toAssembly(gen)) ++ Seq(
            Push(Register(0), Register(1), Register(2), Register(3)),
            LinkBranch("_println"),
            Pop(Register(0), Register(1), Register(2), Register(3))
        ) :+ Comment("end println")
    }
}

object Println extends ParserBridge1[Expr, Println]

case class If(p: Expr, x: List[Stat], y: List[Stat]) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {

        // def stack(table: Table): Int = 0.max(table.getSize() - gen.regs.freeRegs.size)
        
        /*each block is a scope in itself and therefore needs the stack to grow for that block and shrink once execute */

        val cond = p.toAssembly(gen)
        // println(table.ifCount)
        val thenTable = table.getTable(s"_if${table.ifCount}") match {
            case Some(x) => x
            case None => ???
        }
        thenTable.resetCounts()
        val thenBlock = x.map(_.toAssembly(gen)(thenTable)).foldLeft(Seq[Instruction]())(_ ++ _)
        val elseTable = table.getTable(s"_else${table.ifCount}") match {
            case Some(x) => x
            case None => ???
        }
        elseTable.resetCounts()
        val elseBlock = y.map(_.toAssembly(gen)(elseTable)).foldLeft(Seq[Instruction]())(_ ++ _)

        val thenLabel = gen.labels.generate()
        val endLabel = gen.labels.generate()

        table.ifCount += 1

        return If.generateIf(cond, thenLabel, thenBlock, elseBlock, endLabel)
    }

    // override def toString(): String = s"if ($p)\nthen\n${x.mkString("\n")}\nelse\n${x.mkString("\n")}"
    
}

object If extends ParserBridge3[Expr, List[Stat], List[Stat], If] {
    /*method for generating assembly for if statements, which is also used to generate assembly for methods such as printing booleans*/
    def generateIf(cond: Assembly, thenLabel: String, thenBlock: Seq[Instruction], elseBlock: Seq[Instruction], endLabel: String): Seq[Instruction] = {
        var branch: Seq[Instruction] = if (cond.cond == Condition.NO) Seq() else Seq(Branch(thenLabel, cond.cond))
        cond.op match {
            case Some(x) => x match {
                case x: Register => {
                    branch = Seq(Cmp(x, ImmInt(1)), Branch(thenLabel, Condition.EQ))
                }
                case x => 
            }
            case None => 
        }
        
        cond.instr ++ branch ++ 
        elseBlock ++ Seq(Branch(endLabel), Label(thenLabel)) ++ 
        thenBlock :+ Label(endLabel)
    }

}

case class While(p: Expr, x: List[Stat]) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        
        val cond = p.toAssembly(gen)
        // println(table.whileCount)
        val childTable = table.getTable(s"_while${table.whileCount}") match {
            case Some(x) => x
            case None => ???
        }
        childTable.resetCounts()

        val startLabel = gen.labels.generate()
        val endLabel = gen.labels.generate()
        childTable.whileLabels = Some((startLabel, endLabel))

        val block = x.map(_.toAssembly(gen)(childTable)).foldLeft(Seq[Instruction]())(_ ++ _)
        var branch: Seq[Instruction] = if (cond.cond == Condition.AL) Seq() else Seq(Branch(endLabel, Condition.invert(cond.cond)))
        cond.op match {
            case Some(x) => x match {
                case x: Register => {
                    branch = Seq(Cmp(x, ImmInt(0)), Branch(endLabel, Condition.EQ))
                }
                case x => 
            }
            case None => 
        }

        table.whileCount += 1

        return ((Label(startLabel) +: cond.instr) ++ branch ++ block ++ Seq(Branch(startLabel), Label(endLabel)))
    }
}

object While extends ParserBridge2[Expr, List[Stat], While]

case class Begin(xs: List[Stat]) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {

        val childTable = table.getTable(s"_begin${table.beginCount}") match {
            case Some(x) => x
            case None => ???
        }
        childTable.resetCounts()
        val instr = xs.map(_.toAssembly(gen)(childTable)).fold(Seq())(_ ++ _)
        table.beginCount += 1

        childTable.table.toSeq.foreach(entry => entry._2 match {
            case OpSymbol(t, op) => op match {
                case r: Register => gen.regs.free(r)
                case _ => 
            }
            case _ =>  
        })
        return instr
    }
}

object Begin extends ParserBridge1[List[Stat], Begin]