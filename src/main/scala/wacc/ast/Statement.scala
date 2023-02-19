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
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq()
}

object Declare extends ParserBridge3[Type, String, RValue, Declare]

case class Assign(x: LValue, y: RValue) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq() 
}

object Assign extends ParserBridge2[LValue, RValue, Assign]

case class Read(x: LValue) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq() 
}

object Read extends ParserBridge1[LValue, Read]

case class Free(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq() 
}


object Free extends ParserBridge1[Expr, Free]

case class Return(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq() 
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
        val ass = x.toAssembly(regs, symbolTable)
        Seq(
        Mov(Register(0), ass.getOp),
        LinkBranch("printf")) 
    } 
}

object Print extends ParserBridge1[Expr, Print]

case class Println(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq() 
}

object Println extends ParserBridge1[Expr, Println]

case class If(p: Expr, x: List[Stat], y: List[Stat]) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq() 
    
}

object If extends ParserBridge3[Expr, List[Stat], List[Stat], If]

case class While(p: Expr, x: List[Stat]) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq() 
}

object While extends ParserBridge2[Expr, List[Stat], While]

case class Begin(xs: List[Stat]) extends Stat {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq()
}

object Begin extends ParserBridge1[List[Stat], Begin]