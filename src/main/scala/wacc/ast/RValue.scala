package wacc
package ast

import wacc.back._
import wacc.front.ParserBridge._
import parsley.genericbridges._

/* right values as a sealed trait with a position attribute */
trait RValue {
    def pos: (Int, Int)
    def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = TODOAssembly
}

/* case classes for right values */
case class ArrayLiteral(xs: List[Expr])(val pos: (Int, Int)) extends RValue {
    def toInstructions(gen: CodeGenerator, out: RegAssembly)(implicit table: Table): Seq[Instruction] = {
        val assemblies = xs.map(x => x.toAssembly(gen))
        val instrs = (assemblies.map(x => x.instr)).flatten
        val ops = (assemblies.map(x => x.getOp))
        val arrAssembly = gen.heap.mallocArray(ops, out.getReg)
        return (instrs ++ out.instr ++ arrAssembly.instr)
    }
}

/* companion objects for right values */
object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral]

case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends RValue {
        override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {
        val out = gen.regs.allocate
        val assembly1 = fst.toAssembly(gen)
        val assembly2 = snd.toAssembly(gen)
        val pairAssembly = gen.heap.mallocPair(assembly1.getOp, assembly2.getOp, out.getReg)
        return Assembly(out.getReg, assembly1.instr ++ assembly2.instr ++ out.instr ++ pairAssembly.instr)
    }
}

object NewPair extends ParserBridge2[Expr, Expr, NewPair] {
    def apply(fst: Expr, snd: Expr) = new NewPair(fst, snd)(fst.pos, snd.pos)
}

case class Call(id: String, args: List[Expr])(val pos: (Int, Int)) extends RValue {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {
        val out = args.map(_.toAssembly(gen))
        val func = Func.callFunction(id, args = out.map(_.getOp), gen = gen)
        Assembly(Register(0), out.map(_.instr).fold(Seq())(_ ++ _) ++ func)
    }
    // {
        // val branch = LinkBranch(s"wacc_$id")
        // if (args.isEmpty) return Assembly(Register(0), Seq(branch))
        // val regsToSave = (1 until args.length + 1).map(Register(_))

        // def paramToReg(i: Int): Seq[Instruction] = {
        //     val param = args(i)
        //     val reg = regsToSave(i)
        //     val expr = param.toAssembly(gen)
        //     if (expr.getOp == reg) return expr.instr
        //     else return expr.instr :+ Mov(reg, expr.getOp)
        // }

        // val params = (0 until args.length).map(paramToReg).fold(Seq[Instruction]())(_ ++ _)
        
        // return Assembly(Register(0), (gen.regs.save(regsToSave:_*) +: params) ++ Seq(branch, gen.regs.restore(regsToSave:_*)))
    // }
}

object Call extends ParserBridgePos2[String, List[Expr], Call]