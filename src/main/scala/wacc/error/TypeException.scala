package wacc.error

final case class TypeException(private val message: String, private val pos: Seq[(Int, Int)]) extends CompilerException(message, 200) {
    override def toString: String = "pos: " + pos + "\n" + message
}

object TypeException {
    def apply(msg: String): TypeException = return TypeException(msg, Seq())
    def apply(msg: String, pos: (Int, Int)*): TypeException = return new TypeException(msg, pos)
}
