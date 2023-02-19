package wacc.ast

import wacc.front.ParserBridge._
import wacc.back._

/* function case class with position */
case class Func(fs: (Type, String), args: List[Param], stats: List[Stat])(val pos: (Int, Int)) {

    def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = {
        // TODO: function assembly
        val assStat = stats.map(_.toAssembly(regs, symbolTable))
        return Seq()
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
object Func extends ParserBridgePos3[(Type, String), List[Param], List[Stat], Func]

/* parameter case class with position */
case class Param(t: Type, id: String)(val pos: (Int, Int)) {
    def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Seq[Instruction] = Seq() // TODO: param to assembly
}

object Param extends ParserBridgePos2[Type, String, Param]

