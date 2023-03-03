package wacc
package back

import wacc.back._
import Condition._
import ast.Func


abstract class DataSection {
    def toAssembly(): Seq[Instruction]
}

/*assembly method that can be generalised to print characters, integers, 
strings, pointers and new lines*/
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
            LinkBranch("fflush"),
            Pop(PC)
        ))
    }
}

case object PrintCharSection extends PrintSection("c", "%c", "char")
case object PrintIntSection extends PrintSection("i", "%d", "int")
case object PrintStringSection extends PrintSection("s", "%.*s", "str") 
case object PrintNewLine extends PrintSection("ln", "\\n", "ln")
case object PrintPointerSection extends PrintSection("p", "%p", "p")


/*assembly method checks value in register to see if it is a 1 or a 0 and 
accordingly prints the boolean value*/
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
                LinkBranch("_prints"),
                Pop(PC)
            )
        )
    }
}

/*general template for loading 4 byte elems, storing 4 byte elems, 
loading 1 byte and storing 1 byte*/
sealed abstract class ArraySection(id: String, index: Register, array: Register, instr: Instruction) extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        return Func.generateFunction(id, Seq(
            Cmp(index, ImmInt(0)),
            Mov(Register(1), index, Condition.LT),
            LinkBranch("_boundsCheck", Condition.LT),
            Load(LR, Address(array, ImmInt(-4))),
            Cmp(index, LR),
            Mov(Register(1), index, Condition.GE),
            LinkBranch("_boundsCheck", Condition.GE),
            instr,
            Pop(PC)
        ))
    }
}

case object ArrayLoadSection extends ArraySection(
    "_arrLoad",
    Register(2),
    Register(1),
    Load(Register(0), Address(Register(1), LSL(Register(2), ImmInt(2))))
)

case object ArrayStoreSection extends ArraySection(
    "_arrStore",
    Register(2),
    Register(1),
    Store(Register(3), Address(Register(1), LSL(Register(2), ImmInt(2))))
)

case object ArrayLoadBSection extends ArraySection(
    "_arrLoadB",
    Register(2),
    Register(1),
    Load(Register(0), Address(Register(1), Register(2)), true)
)

case object ArrayStoreBSection extends ArraySection(
    "_arrStoreB",
    Register(2),
    Register(1),
    Store(Register(3), Address(Register(1), Register(2)), false, true)
)

/*assembly method to read integers*/
case object ReadIntSection extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 2"),
            Label(".L._readi_str0"),
            Directive(".asciz \"%d\""),
            Section(".text")
        ) ++ Func.generateFunction("_readi", Seq(
            Push(Register(0), Register(1)),
            Mov(Register(1), SP),
            Load(Register(0), DataLabel(".L._readi_str0")),
            LinkBranch("scanf"),
            Load(Register(0), Address(SP, ImmInt(0))),
            Add(SP, SP, ImmInt(4)),
            Pop(Register(1)),
            Pop(PC)
        ))
    }
}

/*assembly method to read characters*/
case object ReadCharSection extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 3"),
            Label(".L._readc_str0"),
            Directive(".asciz \" %c\""),
            Section(".text")
        ) ++ Func.generateFunction("_readc", Seq(
            Push(Register(1)),
            Store(Register(0), Address(SP, ImmInt(-1)), true, true),
            Mov(Register(1), SP),
            Load(Register(0), DataLabel(".L._readc_str0")),
            LinkBranch("scanf"),
            Load(Register(0), Address(SP, ImmInt(0)), true),
            Add(SP, SP, ImmInt(1)),
            Pop(Register(1)),
            Pop(PC)
        ))
    }
}

/*assembly method to free pair sections */
case object FreePairSection extends DataSection {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Label("_freepair"),
            Push(LR),
            Push(Register(8)),
            Mov(Register(8), Register(0)),
            Cmp(Register(8), ImmInt(0)),
            LinkBranch("_errNull", EQ),
            Load(Register(0), Address(Register(8), ImmInt(0))),
            LinkBranch("free"),
            Load(Register(0), Address(Register(8), ImmInt(4))),
            LinkBranch("free"),
            Mov(Register(0), Register(8)),
            LinkBranch("free"),
            Pop(PC, Register(8))
        )
    }
}
