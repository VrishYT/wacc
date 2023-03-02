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
        rhs match {
            case arrLit@ArrayLiteral(_) => {
                var out = gen.regs.allocate(id)
                if (out.getReg().i == 0) {
                    out = gen.regs.allocate(id) // reallocate to make sure r0 is not clobbered
                }
                return arrLit.toInstructions(gen, out)
            }

            case _ => {
                val assembly = rhs.toAssembly(gen).condToReg(gen.regs)
                val reg = Operands.opToReg(assembly.getOp(), gen.regs)
                table.update(id, reg.getReg())
                return assembly.instr ++ reg.instr 
            } 
        }
    }
}

object Declare extends ParserBridge3[Type, String, RValue, Declare]

case class Assign(x: LValue, y: RValue) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        val rhsAssembly = y.toAssembly(gen).condToReg(gen.regs)
        val lval = x match {
            case arrElem@ArrayElem(_, _) => {
                val arrElemAss = arrElem.toAssemblyStore(gen, rhsAssembly)
                return rhsAssembly.instr ++ arrElemAss.instr
            }
            case _ => x.toAssembly(gen)
        }

        y match {
            case StrLiteral(string) => {
                return Assign.assDec(lval, rhsAssembly, gen.regs)
            }
            case _ => {
                val out = gen.regs.allocate
                x match {
                    case Fst(x) => {
                        gen.postSections.addOne(NullDereference)
                        val pairAss = x.toAssembly(gen)
                        val pairReg = pairAss.getReg()
                        return Seq(
                            Push(Register(8))
                        ) ++ 
                        rhsAssembly.instr ++ 
                        pairAss.instr ++ 
                        out.instr ++ 
                        Seq(
                            Mov(Register(8), pairReg), 
                            Cmp(Register(8), ImmInt(0)), 
                            LinkBranch("_errNull", Condition.EQ), 
                            Mov(out.getReg(), rhsAssembly.getOp()), 
                            Load(Register(8), Address(pairReg, ImmInt(0))), 
                            Store(out.getReg(), Address(Register(8), ImmInt(0))), 
                            Pop(Register(8))
                        )
                    }
                    case Snd(x) => {
                        gen.postSections.addOne(NullDereference)
                        val pairAss = x.toAssembly(gen)
                        val pairReg = pairAss.getReg()
                        return Seq(
                            Push(Register(8))
                        ) ++ 
                        rhsAssembly.instr ++ 
                        pairAss.instr ++ 
                        out.instr ++ 
                        Seq(
                            Mov(Register(8), pairReg), 
                            Cmp(Register(8), ImmInt(0)), 
                            LinkBranch("_errNull", Condition.EQ),
                            Mov(out.getReg(), rhsAssembly.getOp()), 
                            Load(Register(8), Address(pairReg, ImmInt(4))), 
                            Store(out.getReg(), 
                            Address(Register(8), ImmInt(0))), Pop(Register(8))
                        )
                    }
                    case _ => {
                        return Assign.assDec(lval, rhsAssembly, gen.regs)
                    }
                }
            }
        }
    }
}


object Assign extends ParserBridge2[LValue, RValue, Assign] {
    def assDec(lval: RegAssembly, rval: Assembly, regs: RegisterAllocator)(implicit table: Table) : Seq[Instruction] = {
        val reg = Operands.opToReg(rval.getOp(), regs)
        return (rval.instr ++ lval.instr ++ reg.instr) :+ Mov(lval.getReg(), reg.getOp())
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
                        return ass.instr ++ Seq(
                            Push(Register(12)),
                            Push(Register(0), Register(1)),
                            Mov(Register(0), ass.getReg()),
                            LinkBranch("_readi"),
                            Mov(Register(12), Register(0)),
                            Pop(Register(0), Register(1)),
                            Mov(ass.getReg(), Register(12)),
                            Pop(Register(12))
                        )} 
                    case CharType => 
                        gen.postSections.addOne(ReadCharSection)
                        return ass.instr ++ Seq(
                            Push(Register(0), Register(1)),
                            Mov(Register(0), ass.getReg()),
                            LinkBranch("_readc"),
                            Mov(Register(12), Register(0)),
                            Pop(Register(0), Register(1)),
                            Mov(ass.getReg(), Register(12)),
                            Pop(Register(12))
                        )
                    case _ => Seq()
                }
            }
        }
    }
}

object Read extends ParserBridge1[LValue, Read]

case class Free(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        gen.postSections.addOne(NullDereference)
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(FreePairSection)

        val xAssembly = x.toAssembly(gen)
        val xOp = xAssembly.getOp()

        val instrns = xAssembly.instr ++ Seq(Push(Register(8)), Mov(Register(8), xOp), 
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
                ass.instr ++ printValue(identType, ass.getReg(), gen)
            }
            case int: IntLiteral => {
                val ass = int.toAssembly(gen)
                ass.instr ++ printValue(IntType, ass.getOp(), gen)
            }
            case str: StrLiteral => {
                val ass = str.toAssembly(gen)
                val reg = gen.regs.allocate 
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
                gen.postSections.addOne(PrintStringSection) 
                val identType = table.getType(id) match {
                    case Some(x) => x
                    case None => ???
                }
                val ass = a.toAssembly(gen)
                return ass.instr ++ printValue(identType, ass.getOp(), gen)
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
        
        val cond = p.toAssembly(gen)
        val thenBlock = x.map(_.toAssembly(gen)).foldLeft(Seq[Instruction]())(_ ++ _) // TODO: symbol table
        val elseBlock = y.map(_.toAssembly(gen)).foldLeft(Seq[Instruction]())(_ ++ _) // TODO: symbol table

        val thenLabel = gen.labels.generate()
        val endLabel = gen.labels.generate()

        return If.generateIf(cond, thenLabel, thenBlock, elseBlock, endLabel)
    }
    
}

object If extends ParserBridge3[Expr, List[Stat], List[Stat], If] {
    def generateIf(cond: Assembly, thenLabel: String, thenBlock: Seq[Instruction], elseBlock: Seq[Instruction], endLabel: String): Seq[Instruction] = {
        val branch = if (cond.cond == Condition.NO) Seq() else Seq(Branch(thenLabel, cond.cond))
        cond.instr ++ branch ++ 
        elseBlock ++ Seq(Branch(endLabel), Label(thenLabel)) ++ 
        thenBlock :+ Label(endLabel)
    }
}

case class While(p: Expr, x: List[Stat]) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = {
        
        val cond = p.toAssembly(gen)
        val block = x.map(_.toAssembly(gen)).foldLeft(Seq[Instruction]())(_ ++ _) // TODO: symbol table

        val startLabel = gen.labels.generate()
        val endLabel = gen.labels.generate()
        val branch = if (cond.cond == Condition.AL) Seq() else Seq(Branch(endLabel, Condition.invert(cond.cond)))

        return (Label(startLabel) +: cond.instr) ++ branch ++ block ++ Seq(Branch(startLabel), Label(endLabel))   
    }
}

object While extends ParserBridge2[Expr, List[Stat], While]

case class Begin(xs: List[Stat]) extends Stat {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Seq[Instruction] = Seq()
}

object Begin extends ParserBridge1[List[Stat], Begin]