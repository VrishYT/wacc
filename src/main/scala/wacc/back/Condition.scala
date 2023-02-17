package wacc
package back

object Condition extends Enumeration {
  type Condition = Value
  val EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, LT, GT, LE, GE = Value
  val AL: Value = Value("")
}