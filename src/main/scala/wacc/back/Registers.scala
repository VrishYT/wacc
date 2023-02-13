package wacc.back

sealed trait Register extends Operand
case object SP extends Register /* Stack Pointer */
case object LR extends Register /* Linked Register */
case object PC extends Register /* Program Counter */
case object FP extends Register /* Frame Pointer */
case class Reg(i: Int) extends Register /* General Purpose Registers */