package wacc.back

trait Operand
case class ImmInt(i: Int) extends Operand {
    def arm11: String = "#" + i
    override def toString(): String = arm11
}
case class ImmChar(c: Char) extends Operand {
    def arm11: String = "#" + c.toInt
    override def toString(): String = arm11
}