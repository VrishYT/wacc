package wacc.error

class CompilerException(private val message: String = "", private val cause: Throwable = None.orNull, private val exit: Int = -1) extends Exception(message, cause) {
    override def toString(): String = message
    def quit = {
        println(this)
        sys.exit(exit)
    }
}

object CompilerException {
    def apply(msg: String, exit: Int): CompilerException = new CompilerException(msg, null, exit)
}