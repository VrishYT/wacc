package wacc
package ast

import wacc.back._
import parsley.genericbridges._ 

// TODO: modify AST to take CodeGenerator instances

/* statements as objects extending the sealed trait Stat */
sealed trait Stat {
    def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction]
}

case object Skip extends Stat with ParserBridge0[Stat] {
    def toAssembly(gen: CodeGenerator)(implicit table: Table) = Seq[Instruction]()
}

case class Declare(t: Type, id: String, rhs: RValue) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        val assembly = rhs.toAssembly(gen).condToReg(gen.regs)
        val instr = rhs match {
            case x: Ident => {
                val reg = gen.regs.allocate(id)
                reg.instr :+ Mov(reg.getReg(), assembly.getOp())
            }
            case _ => {
                val reg = Operands.opToReg(assembly.getOp(), gen.regs)
                table.update(id, reg.getReg())
                reg.instr
            }
        }
        return Comment(s"declare $id") +: (assembly.instr ++ instr)
    }
}

object Declare extends ParserBridge3[Type, String, RValue, Declare]

case class Assign(x: LValue, y: RValue) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        val rhsAssembly = y.toAssembly(gen).condToReg(gen.regs)
        val lval = x match {
            case arrElem@ArrayElem(_, _) => return arrElem.toAssemblyStore(gen, rhsAssembly).instr
            case x: PairElem => {
                val addrAssemb = x.getAddr(gen)
                val reg = Operands.opToReg(rhsAssembly.getOp(), gen.regs)
                return (addrAssemb.instr ++ reg.instr ++ rhsAssembly.instr ++ 
                        Seq(Store(reg.getReg(), addrAssemb.getOp())))
            }
            case _ => x.toAssembly(gen)
        }
        y match {
            case StrLiteral(string) => {
                return Assign.assDec(lval, rhsAssembly, gen.regs)
            }
            case _ => {
                return Assign.assDec(lval, rhsAssembly, gen.regs)
            }
        }
    }
}


object Assign extends ParserBridge2[LValue, RValue, Assign] {
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

        val instrns = assembly.instr ++ Seq(Push(Register(8)), Mov(Register(8), xOp), 
                                         Mov(Register(0), Register(8)), LinkBranch("_freepair"), 
                                         Mov(Register(0), ImmInt(0)), Pop( Register(8)))
        return instrns
    }
}


object Free extends ParserBridge1[Expr, Free]

case class Return(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        val expr = x.toAssembly(gen)
        if (expr.getOp() == Register(0)) return expr.instr 
        else return expr.instr ++ Seq(Mov(Register(0), expr.getOp()), Pop(PC))
    }
}

object Return extends ParserBridge1[Expr, Return]

case class Exit(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        val ass = x.toAssembly(gen)
        ass.instr ++ Seq(
                Mov(Register(0), ass.getOp()),
                LinkBranch("exit"))
    }
}

object Exit extends ParserBridge1[Expr, Exit]

case class Print(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        Comment("start print") +: (
        x match {
            case id@Ident(i) => {
                val identType = table.getType(i) match {
                    case Some(x) => x
                    case None => ???
                }
                val ass = id.toAssembly(gen)
                val reg = Operands.opToReg(ass.getOp(), gen.regs)
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
            case unop@UnaryOpExpr(op, _) => {
                val unopType = op.output 
                val ass = unop.toAssembly(gen).condToReg(gen.regs)
                return ass.instr ++ printValue(unopType, ass.getOp(), gen)
            }
            case binop@BinaryOpExpr(op, _, _) => {
                val binopType = op.output 
                val ass = binop.toAssembly(gen).condToReg(gen.regs)
                return ass.instr ++ printValue(binopType, ass.getOp(), gen)
            }
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
                return ass.instr ++ printValue(identType, ass.getOp(), gen)
            }
            case p@PairLiteralNull(_) => {
                gen.postSections.addOne(PrintPointerSection)
                val ass = p.toAssembly(gen)
                return ass.instr ++ Seq(
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
            
            case PairType(_,_) | ArrayType(_) => {
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

        def stack(table: Table): Int = 0.max(table.getSize - gen.regs.freeRegs.size)

        val cond = p.toAssembly(gen)
        val thenTable = table.getTable(s"_if${table.ifCount}") match {
            case Some(x) => x
            case None => ???
        }
        thenTable.resetCounts()
        val thenStack = stack(thenTable)
        val thenBlock = gen.mem.grow(thenStack) +: x.map(_.toAssembly(gen)(thenTable)).foldLeft(Seq[Instruction]())(_ ++ _) :+ gen.mem.shrink(thenStack)
        val elseTable = table.getTable(s"_else${table.ifCount}") match {
            case Some(x) => x
            case None => ???
        }
        elseTable.resetCounts()
        val elseStack = stack(elseTable)
        val elseBlock = gen.mem.grow(elseStack) +: y.map(_.toAssembly(gen)(elseTable)).foldLeft(Seq[Instruction]())(_ ++ _) :+ gen.mem.shrink(elseStack)

        val thenLabel = gen.labels.generate()
        val endLabel = gen.labels.generate()

        table.ifCount += 1

        return If.generateIf(cond, thenLabel, thenBlock, elseBlock, endLabel)
    }
    
}

object If extends ParserBridge3[Expr, List[Stat], List[Stat], If] {
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
        // println(table)
        val childTable = table.getTable(s"_while${table.whileCount}") match {
            case Some(x) => x
            case None => ???
        }
        childTable.resetCounts()
        val block = x.map(_.toAssembly(gen)(childTable)).foldLeft(Seq[Instruction]())(_ ++ _)

        val startLabel = gen.labels.generate()
        val endLabel = gen.labels.generate()
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

        val stack = 0.max(childTable.getSize - gen.regs.freeRegs.size)

        return gen.mem.grow(stack) +: ((Label(startLabel) +: cond.instr) ++ branch ++ block ++ Seq(Branch(startLabel), Label(endLabel))) :+ gen.mem.shrink(stack)
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
        val stack = 0.max(childTable.getSize - gen.regs.freeRegs.size)

        table.beginCount += 1

        return gen.mem.grow(stack) +: instr :+ gen.mem.shrink(stack)
    }
}

object Begin extends ParserBridge1[List[Stat], Begin]