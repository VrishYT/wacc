object ast {
    case class Program(fs: List[Func], stmt: Stats)

    sealed trait Func
    case class Func_(t: Type, v: Ident, args: List[Param], stmt: Stats) extends Func

    sealed trait Param
    case class Param_(x: Type, y: Ident) extends Param
    /* sealed trait ParamList
    case object EmptyParam extends ParamList
    case class ParamCons(t: Type, v: Ident, xs: ParamList)
    */

    sealed trait Stats extends Func /*SHOULD THIS EXTEND FUNCTIONS??*/
    case class Seq(stmt: Stat, stmts: Stats) extends Stats

    sealed trait Stat extends Stats
    case object Skip extends Stat
    case class Read(x: LHSVal) extends Stat
    case class Free(x: Expr) extends Stat
    case class Return(x: Expr) extends Stat
    case class Exit(x: Expr) extends Stat
    case class Print(x: Expr, newln: NewLine) extends Stat
    case class If(p: Expr, x: Stat, y: Stat) extends Stat
    case class While(P: Expr, x: Stat) extends Stat
    case class Begin(x: Stat) extends Stat

    //sealed trait LHSValue extends 
    //sealed trait RHSValue extends

    sealed trait Expr extends Stat
    //case class IntLiteral(x: IntSign, y: Digits) extends Expr
    //case class Bool(x: BoolLiteral) extends Expr
    //case class Char(x: CharLiteral) extends Expr
    //case class Str(x: StringLiteral) extends Expr

    sealed trait IntSign extends IntLiteral
    case object Positive extends IntSign
    case object Negative extends IntSign

    sealed trait BoolLiteral extends Expr
    case object True extends BoolLiteral
    case object False extends BoolLiteral

    sealed trait PairLiteral extends Expr
    case object Null extends PairLiteral

    case class ArrayLiteral(xs: List[Expr]) extends RHSValue



}


