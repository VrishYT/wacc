package wacc
package ast

import wacc.front.ParserBridge._
import wacc.back._
import scala.collection.mutable.ListBuffer

sealed abstract class Func(
    val isPrivate: Boolean,
    val fs: (Type, String), 
    val args: ListBuffer[Param], 
    val stats: List[Stat]
)(val pos: (Int, Int)) {

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

    def toAssembly(gen: CodeGenerator, class_id: String = "")(implicit table: FuncTable): Seq[Instruction] = {

        gen.mem.size = 0.max(table.getSize - gen.regs.freeRegs.size)

        (0 until args.length).foreach(i => {
            val param = args(i)
            gen.regs.link(param.id, Register(i + 1))
            // println(s"link ${param.id} => r${i+1}")
            // println(s"inUse: ${gen.regs.regsInUse}")
            // println(s"free: ${gen.regs.freeRegs}")
        })
        
        val instr = stats.map(_.toAssembly(gen)).fold(Seq())(_ ++ _)

        // println(s"table = ${table.getSize}\nfreeRegs = ${gen.regs.freeRegs.size}")
        // println(s"inUse: ${gen.regs.regsInUse}")
        // println(s"free: ${gen.regs.freeRegs}")

        // println(s"stacksize = $stack")

        return Func.generateFunction(s"wacc_${fs._2}", gen.mem.grow() +: instr, Func.FuncRegs:_*)
    }

}

object Func {

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

/* function case class with position */
case class TypedFunc(
    override val isPrivate: Boolean, 
    override val fs: (Type, String), 
    arguments: List[Param], 
    override val stats: List[Stat]
)(override val pos: (Int, Int)) extends Func(isPrivate, fs, ListBuffer.from(arguments), stats)(pos)

/* function and parameter companion objects with parser bridges */
object TypedFunc extends ParserBridgePos4[Boolean, (Type, String), List[Param], List[Stat], Func] 

/* function case class with position */
case class TypelessFunc(
    override val isPrivate: Boolean,
    val name: String, 
    arguments: List[Param], 
    override val stats: List[Stat]
)(override val pos: (Int, Int)) extends Func(isPrivate, (NoType, name),  ListBuffer.from(arguments), stats)(pos)

/* function and parameter companion objects with parser bridges */
object TypelessFunc extends ParserBridgePos4[Boolean, String, List[Param], List[Stat], Func]

trait Param {
    val id: String
    val pos: (Int, Int)
    val t: Type
}

/* parameter case class with position */
case class TypedParam(t: Type, id: String)(val pos: (Int, Int)) extends Param

object TypedParam extends ParserBridgePos2[Type, String, Param]

/* typeless parameter case class with position */
case class TypelessParam(id: String)(val pos: (Int, Int)) extends Param {
    val t = NoType
}

object TypelessParam extends ParserBridgePos1[String, TypelessParam]
