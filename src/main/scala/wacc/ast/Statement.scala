package wacc
package ast

import wacc.back._
import parsley.genericbridges._ 

/* statements as objects extending the sealed trait Stat */
sealed trait Stat {
    def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq()
}

case object Skip extends Stat with ParserBridge0[Stat]

case class Declare(t: Type, id: String, rhs: RValue) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        val assembly = rhs.toAssembly(regs, symbolTable)
        val out = regs.allocate(id)
        rhs match {
            case StrLiteral(string) => {
                val label = assembly.getOp.toString
                symbolTable.add(id, t, label)
                return (assembly.instr ++ out.instr ++ Seq(Load(out.getReg, DataLabel(label))))}
            case _ => {
                return (assembly.instr ++ out.instr ++ Seq(Mov(out.getReg, assembly.getOp)))} 
        }
    }
}

object Declare extends ParserBridge3[Type, String, RValue, Declare]

case class Assign(x: LValue, y: RValue) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        val rhsAssembly = y.toAssembly(regs, symbolTable)
        val lval = x.toAssembly(regs, symbolTable)

        return (lval.instr ++ rhsAssembly.instr ++ Seq(Mov(lval.getReg, rhsAssembly.getOp)))
    }
}

object Assign extends ParserBridge2[LValue, RValue, Assign]

case class Read(x: LValue) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        x match {
            case id@Ident(i) => {
                val identType = symbolTable.getType(i)
                val ass = id.toAssembly(regs, symbolTable)
                identType match {
                    case IntType => {
                        symbolTable.post.addOne(ReadIntSection)
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
                        symbolTable.post.addOne(ReadCharSection)
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
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq() 
}


object Free extends ParserBridge1[Expr, Free]

case class Return(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        val expr = x.toAssembly(regs, symbolTable)
        if (expr.getOp == Register(0)) return expr.instr 
        else return expr.instr :+ Mov(Register(0), expr.getOp)
    }
}

object Return extends ParserBridge1[Expr, Return]

case class Exit(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        val ass = x.toAssembly(regs, symbolTable)
        ass.instr ++ Seq(
                Mov(Register(0), ass.getOp),
                LinkBranch("exit"))
    }
}

object Exit extends ParserBridge1[Expr, Exit]

case class Print(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        x match {
            case id@Ident(i) => {
                val identType = symbolTable.getType(i)
                val ass = id.toAssembly(regs, symbolTable)
                return ass.instr ++ printValue(identType, ass.getReg(), regs, symbolTable)
            }
            case int: IntLiteral => {
                val ass = int.toAssembly(regs, symbolTable)
                return ass.instr ++ printValue(IntType, ass.getOp(), regs, symbolTable)
            }
            case str: StrLiteral => {
                val ass = str.toAssembly(regs, symbolTable)
                val label = ass.getOp.toString
                val reg = regs.allocate 
                return ass.instr ++ reg.instr ++ (Load(reg.getReg, DataLabel(label)) +: printValue(StringType, reg.getReg, regs, symbolTable))
            }
            case char: CharLiteral => {
                val ass = char.toAssembly(regs, symbolTable)
                return ass.instr ++ printValue(CharType, ass.getOp(), regs, symbolTable)
            }
            case bool: BoolLiteral => {
                val ass = bool.toAssembly(regs, symbolTable)
                return ass.instr ++ printValue(BoolType, ass.getOp(), regs, symbolTable)
            }
            case unop@UnaryOpExpr(op, _) => {
                val unopType = op.output 
                val ass = unop.toAssembly(regs, symbolTable)
                return ass.instr ++ printValue(unopType, ass.getOp(), regs, symbolTable)
            }
            case binop@BinaryOpExpr(op, _, _) => {
                val binopType = op.output 
                val ass = binop.toAssembly(regs, symbolTable)
                return ass.instr ++ printValue(binopType, ass.getOp(), regs, symbolTable)
            }
            case _ => return Seq()
        }
    }



    def printValue(baseType: Type, operand: Operand, regs: RegisterAllocator, symbolTable: SymbolTable) : Seq[Instruction] = {
        baseType match {
            case StringType => {
                symbolTable.post.addOne(PrintStringSection)
                return Seq(
                    Push(Register(0), Register(1), Register(2)),
                    Mov(Register(2), operand),
                    Load(Register(1), Address(Register(2), ImmInt(-4))),
                    LinkBranch("_prints"),
                    Pop(Register(0), Register(1), Register(2))
                )
            }
            case IntType => {
                symbolTable.post.addOne(PrintIntSection)
                return Seq(
                    Push(Register(0), Register(1)),
                    Mov(Register(1), operand),
                    LinkBranch("_printi"),
                    Pop(Register(0), Register(1))
                )
            }
            case CharType => {
                symbolTable.post.addOne(PrintCharSection)
                return Seq(
                    Push(Register(0), Register(1)),
                    Mov(Register(1), operand),
                    LinkBranch("_printc"),
                    Pop(Register(0), Register(1))
                )
            }
            case BoolType => {
                //symbolTable.post.addOne(PrintBoolSection)
                return Seq(
                    Push(Register(0)),
                    Mov(Register(0), operand),
                    LinkBranch("_printb"),
                    Pop(Register(0))
                )
            }
            case _ => return Seq()
        }
    }
}

object Print extends ParserBridge1[Expr, Print]

case class Println(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        symbolTable.post.addOne(PrintNewLine)
        Print(x).toAssembly(regs, symbolTable) ++ Seq(
            Push(Register(0), Register(1)),
            LinkBranch("_println"),
            Pop(Register(0), Register(1))
        ) 
    }
}

object Println extends ParserBridge1[Expr, Println]

case class If(p: Expr, x: List[Stat], y: List[Stat]) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        
        val cond = p.toAssembly(regs, symbolTable)
        val thenBlock = x.map(_.toAssembly(regs, symbolTable)).foldLeft(Seq[Instruction]())(_ ++ _)
        val elseBlock = y.map(_.toAssembly(regs, symbolTable)).foldLeft(Seq[Instruction]())(_ ++ _)

        val thenLabel = symbolTable.generateLabel
        val endLabel = symbolTable.generateLabel

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
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        
        val cond = p.toAssembly(regs, symbolTable).not()
        val block = x.map(_.toAssembly(regs, symbolTable)).foldLeft(Seq[Instruction]())(_ ++ _)

        val startLabel = symbolTable.generateLabel
        val endLabel = symbolTable.generateLabel

        return (Label(startLabel) +: cond.instr :+ Branch(endLabel, cond.cond)) ++ block ++ Seq(Branch(startLabel), Label(endLabel))   
    }
}

object While extends ParserBridge2[Expr, List[Stat], While]

case class Begin(xs: List[Stat]) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq()
}

object Begin extends ParserBridge1[List[Stat], Begin]