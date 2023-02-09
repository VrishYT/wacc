package wacc.error

final case class TypeException(private val message: String = "", private val cause: Throwable = None.orNull) extends CompilerException(message, cause, 200)

object TypeException {
    def apply(msg: String): TypeException = return TypeException(msg, null)
    def apply(msg: String, cause: Throwable): TypeException = return new TypeException(msg, cause)
}
