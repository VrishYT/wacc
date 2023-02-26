package wacc.back

abstract class RuntimeErrors extends DataSection 

case object DivZeroError extends RuntimeErrors {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 40"),
            Label(".L._errDivZero_str0"),
            Directive(".asciz \"fatal error: division or modulo by zero\""),
            Section(".text"),
            Label("_errDivZero"),
            Load(Register(0), DataLabel(".L._errDivZero_str0")),
            LinkBranch("_prints"),
            Mov(Register(0), ImmInt(255)),
            LinkBranch("exit")
        )
    }
}

case object ArrayBoundsCheck extends RuntimeErrors {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 42"),
            Label(".L._boundsCheck_str0"),
            Directive(".asciz \"fatal error: array index %d out of bounds\n\""),
            Section(".text"),
            Label("_boundsCheck"),
            Load(Register(0), DataLabel(".L._boundsCheck_str0")),
            LinkBranch("printf"),
            Mov(Register(0), ImmInt(0)),
            LinkBranch("fflush"),
            Mov(Register(0), ImmInt(255)),
            LinkBranch("exit")
        )
    }
}

case object IntegerOverflow extends RuntimeErrors {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 52"),
            Label(".L._errOverflow_str0"),
            Directive(".asciz \"fatal error: integer overflow or underflow occurred\n\""),
            Section(".text"),
            Label("_errOverflow"),
            Load(Register(0), DataLabel(".L._errOverflow_str0")),
            LinkBranch("_prints"),
            Mov(Register(0), ImmInt(255)),
            LinkBranch("exit")
        )
    }
}

case object NullDereference extends RuntimeErrors {
    def toAssembly(): Seq[Instruction] = {
        return Seq(
            Section(".data"),
            Directive(".word 45"),
            Label(".L._errNull_str0"),
            Directive(".asciz \"fatal error: null pair dereferenced or freed\n\""),
            Section(".text"),
            Label("_errNull"),
            Load(Register(0), DataLabel(".L._errNull_str0")),
            LinkBranch("_prints"),
            Mov(Register(0), ImmInt(255)),
            LinkBranch("exit")
        )
    }
}