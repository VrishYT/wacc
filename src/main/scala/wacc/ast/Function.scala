package wacc
package ast

import wacc.front.ParserBridge._
import wacc.back._

trait Func {
    def pos: (Int, Int)
    def toAssembly(gen: CodeGenerator)(implicit table: FuncTable): Seq[Instruction]
    def validReturn: Boolean
    val fs: (Type, String)
    val args: List[Param]
    val stats: List[Stat]
}

/* function case class with position */
case class TypedFunc(fs: (Type, String), args: List[Param], stats: List[Stat])(val pos: (Int, Int)) extends Func {

    def toAssembly(gen: CodeGenerator)(implicit table: FuncTable): Seq[Instruction] = {

        (0 until args.length).foreach(i => {
            val param = args(i)
            gen.regs.link(param.id, Register(i + 1))
        })
        
        val instr = stats.map(_.toAssembly(gen)).fold(Seq())(_ ++ _)
        val stack = 0.max(table.getSize - gen.regs.freeRegs.size)

        return gen.mem.grow(stack) +: TypedFunc.generateFunction(s"wacc_${fs._2}", instr) :+ gen.mem.shrink(stack)
    }

    /* define validReturn of a function, and match on the last statement : */
    def validReturn: Boolean = validReturn(stats)

    def validReturn(stats: List[Stat]): Boolean = stats.last match {

        /* return true if it's a return or exit */
        case _: Return | _: Exit => true

        /* check the other valid case for begin or if statements */
        case _ => {
            var valid = false

            /* check if each branch of any new scope has a valid return, valid
                        will remain false if they don't */
            stats.foreach(stat => stat match {
                case If(_, x, y) => {
                    valid |= validReturn(x) && validReturn(y)
                }
                case Begin(xs) => {
                    valid |= validReturn(xs)
                }
                case _ =>
            })

            /* return valid once all eligible paths of control flow
                        have been checked */
            valid
            }
    }
}

/* function case class with position */
case class TypelessFunc(name: String, args: List[Param], stats: List[Stat])(val pos: (Int, Int)) extends Func {

    val fs: (Type, String) = (NoType, name)

    def toAssembly(gen: CodeGenerator)(implicit table: FuncTable): Seq[Instruction] = {

        (0 until args.length).foreach(i => {
            val param = args(i)
            gen.regs.link(param.id, Register(i + 1))
        })
        
        val instr = stats.map(_.toAssembly(gen)).fold(Seq())(_ ++ _)
        val stack = 0.max(table.getSize - gen.regs.freeRegs.size)

        return gen.mem.grow(stack) +: TypelessFunc.generateFunction(s"wacc_${name}", instr) :+ gen.mem.shrink(stack)
    }

    /* define validReturn of a function, and match on the last statement : */
    def validReturn: Boolean = validReturn(stats)

    def validReturn(stats: List[Stat]): Boolean = stats.last match {

        /* return true if it's a return or exit */
        case _: Return | _: Exit => true

        /* check the other valid case for begin or if statements */
        case _ => {
            var valid = false

            /* check if each branch of any new scope has a valid return, valid
                        will remain false if they don't */
            stats.foreach(stat => stat match {
                case If(_, x, y) => {
                    valid |= validReturn(x) && validReturn(y)
                }
                case Begin(xs) => {
                    valid |= validReturn(xs)
                }
                case _ =>
            })

            /* return valid once all eligible paths of control flow
                        have been checked */
            valid
            }
    }
}

/* function and parameter companion objects with parser bridges */
object TypedFunc extends ParserBridgePos3[(Type, String), List[Param], List[Stat], TypedFunc] {

    import scala.collection.mutable.{ListBuffer}

    val Func_Regs = Seq(
        Register(1),
        Register(2),
        Register(3),
        Register(4),
        Register(5),
        Register(6),
        Register(7),
        Register(8),
        Register(9),
        Register(10)
    )

    def generateFunction(id: String, instr: Seq[Instruction], regsToSave: Register*): Seq[Instruction] = {
            val pop = if (regsToSave.isEmpty) Seq() else Seq(Pop{regsToSave:_*})
            Seq(
                Label(id),
                Push{(LR +: regsToSave):_*}
            ) ++ instr ++ pop
    }

    def callFunction(id: String, args: Seq[Operand] = Seq(), gen: CodeGenerator): Seq[Instruction] = {

        if (args.length > Func_Regs.length) println("Too many args to load in") // TODO: load excess into mem 

        val range = (0 until args.length.min(Func_Regs.length))

        val instr = ListBuffer[Instruction]()
        val regs = range.map(i => Register(i + 1))/*.filter(gen.regs.isAllocated(_))*/

        if (!regs.isEmpty) instr += Push(regs:_*)

        instr ++= range.map(i => args(i) match {
            case x: Register => Mov(Register(i + 1), x)
            case x => Operands.opToReg(x, Register(i + 1)) 
        })

        instr += LinkBranch(id)
        if (!regs.isEmpty) instr += Pop(regs:_*)

        return instr.toSeq
    }

}

/* function and parameter companion objects with parser bridges */
object TypelessFunc extends ParserBridgePos3[String, List[Param], List[Stat], TypelessFunc] {

    import scala.collection.mutable.{ListBuffer}

    val Func_Regs = Seq(
        Register(1),
        Register(2),
        Register(3),
        Register(4),
        Register(5),
        Register(6),
        Register(7),
        Register(8),
        Register(9),
        Register(10)
    )

    def generateFunction(id: String, instr: Seq[Instruction], regsToSave: Register*): Seq[Instruction] = {
            val pop = if (regsToSave.isEmpty) Seq() else Seq(Pop{regsToSave:_*})
            Seq(
                Label(id),
                Push{(LR +: regsToSave):_*}
            ) ++ instr ++ pop
    }

    def callFunction(id: String, args: Seq[Operand] = Seq(), gen: CodeGenerator): Seq[Instruction] = {

        if (args.length > Func_Regs.length) println("Too many args to load in") // TODO: load excess into mem 

        val range = (0 until args.length.min(Func_Regs.length))

        val instr = ListBuffer[Instruction]()
        val regs = range.map(i => Register(i + 1))/*.filter(gen.regs.isAllocated(_))*/

        if (!regs.isEmpty) instr += Push(regs:_*)

        instr ++= range.map(i => args(i) match {
            case x: Register => Mov(Register(i + 1), x)
            case x => Operands.opToReg(x, Register(i + 1)) 
        })

        instr += LinkBranch(id)
        if (!regs.isEmpty) instr += Pop(regs:_*)

        return instr.toSeq
    }

}

/* parameter case class with position */
case class Param(t: Type, id: String)(val pos: (Int, Int))

object Param extends ParserBridgePos2[Type, String, Param]

