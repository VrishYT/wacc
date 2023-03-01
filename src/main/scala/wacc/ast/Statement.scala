package wacc
package ast

import wacc.back._
import parsley.genericbridges._ 

// TODO: modify AST to take CodeGenerator instances

/* statements as objects extending the sealed trait Stat */
sealed trait Stat {
    def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = Seq()
}

case object Skip extends Stat with ParserBridge0[Stat]

case class Declare(t: Type, id: String, rhs: RValue) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        val out = gen.regs.allocate(id)
        rhs match {
            case StrLiteral(string) => {
                val assembly = rhs.toAssembly(gen, table)
                val label = assembly.getOp.toString
                table.update(id, label)
                return (assembly.instr ++ out.instr ++ Seq(Load(out.getReg, DataLabel(label))))
                }
                
            case NewPair(fst, snd) => {
                val assembly1 = fst.toAssembly(gen, table)
                val assembly2 = snd.toAssembly(gen, table)
                val pairAssembly = gen.heapAlloc.mallocPair(assembly1.getOp, assembly2.getOp, out.getReg)
                return (assembly1.instr ++ assembly2.instr ++ out.instr ++ pairAssembly.instr)
            }
            case ArrayLiteral(xs) => {
                val assemblies = xs.map(x => x.toAssembly(gen, table))
                val instrs = (assemblies.map(x => x.instr)).flatten
                val ops = (assemblies.map(x => x.getOp))
                val arrAssembly = gen.heapAlloc.mallocArray(ops, out.getReg)
                return (instrs ++ out.instr ++ arrAssembly.instr)
            }

            case _ => {
                val assembly = rhs.toAssembly(gen, table)
                return (assembly.instr ++ out.instr ++ Seq(Mov(out.getReg, assembly.getOp)))
                } 
        }
    }
}

object Declare extends ParserBridge3[Type, String, RValue, Declare]

case class Assign(x: LValue, y: RValue) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        val rhsAssembly = y.toAssembly(gen, table).condToReg(gen.regs)
        val lval = x.toAssembly(gen, table)
        y match {
            case StrLiteral(string) => {
                val label = rhsAssembly.getOp.toString
                return (rhsAssembly.instr ++ lval.instr ++ Seq(Load(lval.getReg, DataLabel(label))))}
            case _ => {
                val out = gen.regs.allocate
                x match {
                    case Fst(_) => {
                        return (rhsAssembly.instr ++ lval.instr ++ out.instr ++ Seq(Mov(out.getReg, rhsAssembly.getOp), Store(out.getReg, Address(lval.getReg, ImmInt(0)))))}
                    case Snd(_) => {
                        return (rhsAssembly.instr ++ lval.instr ++ out.instr ++ Seq(Mov(out.getReg, rhsAssembly.getOp), Store(out.getReg, Address(lval.getReg, ImmInt(4)))))}
                    case _ => {
                        return (rhsAssembly.instr ++ lval.instr ++ Seq(Mov(lval.getReg, rhsAssembly.getOp)))}
                }
            }
        }
    }
}

object Assign extends ParserBridge2[LValue, RValue, Assign] // TODO refactor this later

case class Read(x: LValue) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        Seq()
        x match {
            case id@Ident(i) => {
                val identType = table.getType(i)
                val ass = id.toAssembly(gen, table)
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
                            Mov(ass.getReg, Register(12)),
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
                            Mov(ass.getReg, Register(12)),
                            Pop(Register(12))
                        )
                    case StringType => Seq()
                }
            }
        }
    }
}

object Read extends ParserBridge1[LValue, Read]

case class Free(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        gen.postSections.addOne(NullDereference)
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(FreePairSection)

        val xAssembly = x.toAssembly(gen, table)
        val xOp = xAssembly.getOp

        val instrns = xAssembly.instr ++ Seq(Push(Register(8), Register(0)), Mov(Register(8), xOp), 
                                         Mov(Register(0), Register(8)), LinkBranch("_freepair"), 
                                         Mov(Register(0), ImmInt(0)), Pop(Register(0), Register(8)))
        return instrns


    }
}


object Free extends ParserBridge1[Expr, Free]

case class Return(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        val expr = x.toAssembly(gen, table)
        if (expr.getOp == Register(0)) return expr.instr 
        else return expr.instr ++ Seq(Mov(Register(0), expr.getOp), Pop(PC))
    }
}

object Return extends ParserBridge1[Expr, Return]

case class Exit(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        val ass = x.toAssembly(gen, table)
        ass.instr ++ Seq(
                Mov(Register(0), ass.getOp),
                LinkBranch("exit"))
    }
}

object Exit extends ParserBridge1[Expr, Exit]

case class Print(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        Comment("start print") +: (
        x match {
            case id@Ident(i) => {
                val identType = table.getType(i) 
                val ass = id.toAssembly(gen, table)
                ass.instr ++ printValue(identType, ass.getReg(), gen)
            }
            case int: IntLiteral => {
                val ass = int.toAssembly(gen, table)
                ass.instr ++ printValue(IntType, ass.getOp(), gen)
            }
            case str: StrLiteral => {
                val ass = str.toAssembly(gen, table)
                val label = ass.getOp.toString
                val reg = gen.regs.allocate 
                ass.instr ++ reg.instr ++ (Load(reg.getReg, DataLabel(label)) +: printValue(StringType, reg.getReg, gen))
            }
            case char: CharLiteral => {
                val ass = char.toAssembly(gen, table)
                ass.instr ++ printValue(CharType, ass.getOp(), gen)
            }
            case bool: BoolLiteral => {
                val ass = bool.toAssembly(gen, table)
                ass.instr ++ printValue(BoolType, ass.getOp(), gen)
            }
            case unop@UnaryOpExpr(op, _) => {
                val unopType = op.output 
                val ass = unop.toAssembly(gen, table).condToReg(gen.regs)
                return ass.instr ++ printValue(unopType, ass.getOp(), gen)
            }
            case binop@BinaryOpExpr(op, _, _) => {
                val binopType = op.output 
                val ass = binop.toAssembly(gen, table).condToReg(gen.regs)
                return ass.instr ++ printValue(binopType, ass.getOp(), gen)
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
            case _ => return Seq()
        }
    }
}

object Print extends ParserBridge1[Expr, Print]

case class Println(x: Expr) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        gen.postSections.addOne(PrintNewLine)
        (Comment("start println") +: Print(x).toAssembly(gen, table)) ++ Seq(
            Push(Register(0), Register(1), Register(2), Register(3)),
            LinkBranch("_println"),
            Pop(Register(0), Register(1), Register(2), Register(3))
        ) :+ Comment("end println")
    }
}

object Println extends ParserBridge1[Expr, Println]

case class If(p: Expr, x: List[Stat], y: List[Stat]) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        
        val cond = p.toAssembly(gen, table)
        val thenBlock = x.map(_.toAssembly(gen, table)).foldLeft(Seq[Instruction]())(_ ++ _) // TODO: symbol table
        val elseBlock = y.map(_.toAssembly(gen, table)).foldLeft(Seq[Instruction]())(_ ++ _) // TODO: symbol table

        val thenLabel = gen.labels.generate
        val endLabel = gen.labels.generate

        return If.generateIf(cond, thenLabel, thenBlock, elseBlock, endLabel)
    }
    
}

object If extends ParserBridge3[Expr, List[Stat], List[Stat], If] {
    def generateIf(cond: Assembly, thenLabel: String, thenBlock: Seq[Instruction], elseBlock: Seq[Instruction], endLabel: String): Seq[Instruction] 
        = (cond.instr :+ Branch(thenLabel, cond.cond)) ++ 
            elseBlock ++ Seq(Branch(endLabel), Label(thenLabel)) ++ 
            thenBlock :+ Label(endLabel)
}

case class While(p: Expr, x: List[Stat]) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = {
        
        val cond = p.toAssembly(gen, table)
        val block = x.map(_.toAssembly(gen, table)).foldLeft(Seq[Instruction]())(_ ++ _) // TODO: symbol table

        val startLabel = gen.labels.generate
        val endLabel = gen.labels.generate
        val branch = if (cond.cond == Condition.AL) Seq() else Seq(Branch(endLabel, Condition.invert(cond.cond)))

        return (Label(startLabel) +: cond.instr) ++ branch ++ block ++ Seq(Branch(startLabel), Label(endLabel))   
    }
}

object While extends ParserBridge2[Expr, List[Stat], While]

case class Begin(xs: List[Stat]) extends Stat {
    override def toAssembly(gen: CodeGenerator, table: Table): Seq[Instruction] = Seq()
}

object Begin extends ParserBridge1[List[Stat], Begin]