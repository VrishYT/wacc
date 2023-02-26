package wacc
package back

import ast.Func

sealed abstract class RuntimeErrors(val label: String, val msg: String) extends DataSection {

    def generateData: Seq[Instruction] = Seq(
        Section(".data"),
        Directive(s".word ${msg.length}"),
        Label(s".L.${label}_err"),
        Directive(s".asciz \"${msg}\""),
        Section(".text")
    )

    def generateFunction: Seq[Instruction] = Func.generateFunction(label, Seq(
        Load(Register(0), DataLabel(s".L.${label}_err")),
        Mov(Register(2), Register(0)),
        Load(Register(1), Address(Register(0), ImmInt(-4))),
        LinkBranch("_prints"),
        Mov(Register(0), ImmInt(-1)),
        LinkBranch("exit")
    ))

    def toAssembly(): Seq[Instruction] = generateData ++ generateFunction
} 

case object DivZeroError extends RuntimeErrors ("_errDivZero", "fatal error: division or modulo by zero")
case object ArrayBoundsCheck extends RuntimeErrors ("_boundsCheck", "fatal error: array index %d out of bounds")
case object IntegerOverflow extends RuntimeErrors ("_errOverflow", "fatal error: integer overflow or underflow occurred")
case object NullDereference extends RuntimeErrors ("_errNull", "fatal error: null pair dereferenced or freed")