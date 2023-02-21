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
            Directive(s".asciz \"%${format}\""),
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

case object PrintCharSection extends PrintSection("c", "c", "char")
case object PrintIntSection extends PrintSection("c", "d", "int")
case object PrintStringSection extends PrintSection("s", ".*s", "str") 
case object PrintNewLine extends PrintSection("ln", "\n", "ln")

// TODO: ??? can this just "bl _prints" from PrintStringSection with "true" or "false" ???
case object PrintBoolSection {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 5"),
            Label(".L.printb_false"),
            Directive(".asciz \"false\""),
            Directive(".word 4"),
            Label(".L.printb_true"),
            Directive(".asciz \"true\""),
            Directive(".word 4"),
            Label(".L.printb_str"),
            Directive(".asciz \"%.*s\""),
            Section(".text"),
            Label("_printb"),
            Push(LR),
            // TODO: ??? maybe abstract IF generation for use here and in IF statement ???
            Cmp(Register(0), ImmInt(0)),
            Branch(".L_printb_true", NE),
            Load(Register(2), DataLabel(".L.printb_false")),
            Branch(".L_printb_fi"),
            Label(".L_printb_true"),
            Load(Register(2), DataLabel(".L.printb_true")),
            Label(".L_printb_fi"),
            Load(Register(1), Address(Register(2), ImmInt(-4))),
            Load(Register(0), DataLabel(".L.printb_str")),
            LinkBranch("printf"),
            Mov(Register(0), ImmInt(0)),
            LinkBranch("fflush"),
            Pop(PC)

        )
    }
}


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
