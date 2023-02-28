package wacc
package back

import scala.collection.mutable.{Map => MapM}
import wacc.back._
import Condition._
import ast.Func

// TODO: modify all assembly to not clobber registers when printing
// JAMIE LEAVES CERTAIN REGISTERS EMPTY WHILE WE DONT

abstract class DataSection {
    def toAssembly(): Seq[Instruction]
}

sealed abstract class PrintSection(val short: String, val format: String, val name: String) extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        val label = s".L._print${short}_${name}"
        val size = format.length
        return Seq(
            Section(".data"),
            Directive(s".word $size"),
            Label(label),
            Directive(s".asciz \"${format}\""),
            Section(".text")
        ) ++ Func.generateFunction(s"_print${short}", Seq(
            Load(Register(0), DataLabel(label)),
            LinkBranch("printf"),
            Mov(Register(0), ImmInt(0)),
            LinkBranch("fflush")
        ))
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
            Section(".text")
        ) ++ Func.generateFunction("_printb", 
            wacc.ast.If.generateIf(
                Assembly(Seq(Cmp(Register(0), ImmInt(0))), NE), 
                ".L_printb_true",
                Seq(Load(Register(2), DataLabel(".L.printb_t"))),
                Seq(Load(Register(2), DataLabel(".L.printb_f"))),
                ".L_printb_fi"
            ) ++ Seq(
                Load(Register(1), Address(Register(2), ImmInt(-4))),
                LinkBranch("_prints")
            )
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
            Section(".text")
        ) ++ Func.generateFunction("_readi", Seq(
            Push(Register(0)),
            Mov(Register(1), SP),
            Load(Register(0), DataLabel(".L._readi_str0")),
            LinkBranch("scanf"),
            Load(Register(0), Address(SP, ImmInt(0))),
            Add(SP, SP, ImmInt(4))
        ), Register(1))
    }
}

case object ReadCharSection extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 3"),
            Label(".L._readc_str0"),
            Directive(".asciz \" %c\""),
            Section(".text")
        ) ++ Func.generateFunction("_readc", Seq(
            Push(Register(0)),
            Mov(Register(1), SP),
            Load(Register(0), DataLabel(".L._readc_str0")),
            LinkBranch("scanf"),
            Load(Register(0), Address(SP, ImmInt(0))),
            Add(SP, SP, ImmInt(1))
        ), Register(1))
    }
}

class TextSection extends DataSection {

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
}
