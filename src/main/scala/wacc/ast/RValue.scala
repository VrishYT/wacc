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

case class Call(id: List[String], args: List[Expr])(val pos: (Int, Int)) extends RValue {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {
        val out = args.map(_.toAssembly(gen))
        val func = Func.callFunction(s"wacc_${id}", args = out.map(_.getOp()), gen = gen)
        Assembly(Register(0), out.map(_.instr).fold(Seq())(_ ++ _) ++ func)
    }
}

object Call extends ParserBridgePos2[List[String], List[Expr], Call]