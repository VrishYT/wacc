package wacc.ast

import wacc.back._
import wacc.front.ParserBridge._
import parsley.genericbridges._

/* right values as a sealed trait with a position attribute */
trait RValue {
    def pos: (Int, Int)
    def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Assembly = TODOAssembly
}

/* case classes for right values */
case class ArrayLiteral(xs: List[Expr])(val pos: (Int, Int)) extends RValue

/* companion objects for right values */
object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral]

case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends RValue

object NewPair extends ParserBridge2[Expr, Expr, NewPair] {
    def apply(fst: Expr, snd: Expr) = new NewPair(fst, snd)(fst.pos, snd.pos)
}

case class Call(id: String, args: List[Expr])(val pos: (Int, Int)) extends RValue {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Assembly = {
        val branch = LinkBranch(s"wacc_$id")
        if (args.isEmpty) return Assembly(Register(0), Seq(branch))
        val regsToSave = (1 until args.length + 1).map(Register(_))

        def paramToReg(i: Int): Seq[Instruction] = {
            val param = args(i)
            val reg = regsToSave(i)
            val expr = param.toAssembly(regs, symbolTable)
            if (expr.getOp == reg) return expr.instr
            else return expr.instr :+ Mov(reg, expr.getOp)
        }

        val params = (0 until args.length).map(paramToReg).fold(Seq[Instruction]())(_ ++ _)
        
        return Assembly(Register(0), (regs.save(regsToSave:_*) +: params) ++ Seq(branch, regs.restore(regsToSave:_*)))
    }
}

object Call extends ParserBridgePos2[String, List[Expr], Call]