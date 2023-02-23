package wacc.back

import scala.collection.mutable.{Map => MapM}
import wacc.back._
import Condition._

// TODO: modify all assembly to not clobber registers when printing
// JAMIE LEAVES CERTAIN REGISTERS EMPTY WHILE WE DONT

sealed abstract class DataSection {
    def toAssembly(): Seq[Instruction]
}

sealed abstract class PrintSection(val short: String, val format: String, val name: String) extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        val label = s".L._print${short}_${name}"
        val size = format.length + 1
        return Seq(
            Section(".data"),
            Directive(s".word $size"),
            Label(label),
            Directive(s".asciz \"${format}\""),
            Section(".text"),
            Label(s"_print${short}"),
            Push(LR),
            Load(Register(0), DataLabel(label)),
            LinkBranch("printf"),
            Mov(Register(0), ImmInt(0)),
            LinkBranch("fflush"),
            Pop(PC)
        )
    }
}

case object PrintCharSection extends PrintSection("c", "%c", "char")
case object PrintIntSection extends PrintSection("i", "%d", "int")
case object PrintStringSection extends PrintSection("s", "%.*s", "str") 
case object PrintNewLine extends PrintSection("ln", "\\n", "ln")

case object PrintBoolSection extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 5"),
            Label(".L.printb_f"),
            Directive(".asciz \"false\""),
            Directive(".word 4"),
            Label(".L.printb_t"),
            Directive(".asciz \"true\""),
            Section(".text"),
            Label("_printb"),
            Push(LR)
        ) ++ wacc.ast.If.generateIf(
            Assembly(Seq(Cmp(Register(0), ImmInt(0))), NE), 
            ".L_printb_true",
            Seq(Load(Register(2), DataLabel(".L.printb_t"))),
            Seq(Load(Register(2), DataLabel(".L.printb_f"))),
            ".L_printb_fi"
        ) ++ Seq(
            Load(Register(1), Address(Register(2), ImmInt(-4))),
            LinkBranch("_prints"),
            Pop(PC)
        )
    }
}


case object ReadIntSection extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 2"),
            Label(".L._readi_str0"),
            Directive(".asciz \"%d\""),
            Section(".text"),
            Label("_readi"),
            Push(LR),
            Push(Register(0)),
            Mov(Register(1), SP),
            Load(Register(0), DataLabel(".L._readi_str0")),
            LinkBranch("scanf"),
            Load(Register(0), Address(SP, ImmInt(0))),
            Add(SP, SP, ImmInt(4)),
		    Pop(PC)
        )
    }
}

case object ReadCharSection extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 3"),
            Label(".L._readc_str0"),
            Directive(".asciz \" %c\""),
            Section(".text"),
            Label("_readc"),
            Push(LR),
            Push(Register(0)),
            Mov(Register(1), SP),
            Load(Register(0), DataLabel("=.L._readc_str0")),
            LinkBranch("scanf"),
            Load(Register(0), Address(SP, ImmInt(0))),
            Add(SP, SP, ImmInt(1)),
		    Pop(PC)
        )
    }
}

// case object ReadCharSection extends DataSection {
//     def toAssembly(): Seq[Instruction] = {
//         return Seq{
//             Section(".data"),
//             Directive(".word 3"),
//             Label(".L._readc_str0"),
// 		    Directive(".asciz \" %c\""),
//             Section(".text"),
//             Label("_readc"),
// 		    Push(LR),
//             Push(Register(0)),
//             Mov(Register(1), SP),
//             Load(Register(0), DataLabel("=.L._readc_str0")),
//             LinkBranch("scanf"),
// 		    Load(Register(0), Address(SP, ImmInt(0))),
// 		    Add(SP, SP, ImmInt(1)),
// 		    Pop(PC)
//         }
//     }
// }


class TextSection extends DataSection {

    // TODO: change key type to label ???
    private val table = MapM[String, String]()
    private var counter = 0

    def add(value: String): String = {
        val label = ".L.str" + counter
        table(label) = value
        counter += 1
        return label
    }

    def get(id: String): String = table.get(id) match {
        case Some(x) => x
        case None => ???
    }

    override def toAssembly(): Seq[Instruction] = {
        if (table.isEmpty) return Seq()

        def entryToAssembly(entry: (String, String)): Seq[Instruction] = {
            val label = entry._1
            val data = entry._2

            return Seq(
                Directive(s".word ${data.length}"),
                Label(label),
                Directive(s".asciz \"${data}\"")
            )
        }

        return Section(".data") +: table.map(entryToAssembly).fold(Seq())(_ ++ _) :+ Section(".text")

    }

    // override def toString(): String = {
    //     if (table.isEmpty) return ""

    //     // TODO: update according to other TODO's related to DataSection
    //     def entryToAssembly(entry: (String, String)): String = {
    //         val label = entry._1
    //         val data = entry._2

    //         return s"\t.word ${data.length()}\n" + 
    //                label + ":\n" +
    //                s"\t.asciz \"${data}\""
    //     }

    //     return ".data\n" + 
    //            table.toSeq.map(entryToAssembly).mkString +
    //            ".text\n"
    // }
    
}
