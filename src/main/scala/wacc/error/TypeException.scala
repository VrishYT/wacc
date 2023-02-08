package wacc.error

final case class TypeException(private val pos: (Int, Int), private val message: String) extends CompilerException(message, 200) {
    override def toString: String = "pos: " + pos + "\n" + message
}

object TypeException {
    def apply(msg: String): TypeException = return TypeException((0,0), msg)
    def apply(pos: (Int, Int), msg: String): TypeException = return new TypeException(pos, msg)
}
