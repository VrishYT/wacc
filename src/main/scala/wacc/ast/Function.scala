package wacc
package ast

import wacc.front.ParserBridge._
import wacc.back._
import scala.collection.mutable.ListBuffer

/* function case class with position */
case class Func(fs: (Type, String), args: List[Param], stats: List[Stat])(val pos: (Int, Int)) {

    def toAssembly(gen: CodeGenerator, table: FuncTable): Seq[Instruction] = {
        // TODO: function assembly

        (1 until args.length + 1).foreach(i => {
            val param = args(i-1)
            gen.regs.link(param.id, Register(i))
        })
        
        val instr = stats.map(_.toAssembly(gen, table)).fold(Seq())(_ ++ _)

        // return Seq(Label(s"wacc_${fs._2}"), Push(LR)) ++ instr :+ Pop(PC)
        return Func.generateFunction(s"wacc_${fs._2}", instr)
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
object Func extends ParserBridgePos3[(Type, String), List[Param], List[Stat], Func] {

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
        val regs = range.map(Register(_)).filter(gen.regs.isAllocated(_))

        if (!regs.isEmpty) instr += Push(regs:_*)

        instr ++= range.map(i => args(i) match {
            case x: Register => Mov(Register(i + 1), x)
            case x => Operands.opToReg(x, Register(i + 1)) 
        })

        instr += LinkBranch(s"wacc_${id}")
        if (!regs.isEmpty) instr += Pop(regs:_*)

        return instr.toSeq
    }

}

/* parameter case class with position */
case class Param(t: Type, id: String)(val pos: (Int, Int))

object Param extends ParserBridgePos2[Type, String, Param]

