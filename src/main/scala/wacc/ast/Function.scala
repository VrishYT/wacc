package wacc
package ast

import wacc.front.ParserBridge._
import wacc.back._
import scala.collection.mutable.ListBuffer

/* function case class with position */
case class Func(fs: (Type, String), args: List[Param], stats: List[Stat])(val pos: (Int, Int)) {

    def toAssembly(gen: CodeGenerator)(implicit table: FuncTable): Seq[Instruction] = {

        gen.mem.size = 0.max(table.getSize - gen.regs.freeRegs.size)

        (0 until args.length).foreach(i => {
            val param = args(i)
            gen.regs.link(param.id, Register(i + 1))
            println(s"link ${param.id} => r${i+1}")
            println(s"inUse: ${gen.regs.regsInUse}")
            println(s"free: ${gen.regs.freeRegs}")
        })
        
        val instr = stats.map(_.toAssembly(gen)).fold(Seq())(_ ++ _)

        println(s"table = ${table.getSize}\nfreeRegs = ${gen.regs.freeRegs.size}")
        println(s"inUse: ${gen.regs.regsInUse}")
        println(s"free: ${gen.regs.freeRegs}")

        // println(s"stacksize = $stack")

        return Func.generateFunction(s"wacc_${fs._2}", gen.mem.grow() +: instr, Func.FuncRegs:_*)
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

    val FuncRegs = Seq(
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
            // val pop = if (regsToSave.isEmpty) Seq() else Seq(Pop{regsToSave:_*})
            Seq(
                Label(id),
                Push{(regsToSave :+ LR):_*}
            ) ++ instr
    }

    def callFunction(id: String, args: Seq[Operand] = Seq(), gen: CodeGenerator): Seq[Instruction] = {

        if (args.length > FuncRegs.length) println("Too many args to load in") // TODO: load excess into mem 

        val range = (0 until args.length.min(FuncRegs.length))

        val instr = ListBuffer[Instruction]()
        val regs = range.map(i => Register(i + 1)).filter(gen.regs.isAllocated(_))

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

