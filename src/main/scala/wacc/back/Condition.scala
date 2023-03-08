package wacc
package back

object Condition extends Enumeration {
  type Condition = Value
  val EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, LT, GT, LE, GE = Value
  val AL: Value = Value("")
  val NO: Value = Value("") // USED INTERNALLY ONLY

  /*helper method for negating the conditions*/
  def invert(cond: Condition): Condition = cond match {
    case AL => NO
    case NO => AL
    case EQ => NE
    case NE => EQ
    case LT => GE
    case GT => LE
    case GE => LT
    case LE => GT
    case x => x // TODO: figure out other match cases
  }

}