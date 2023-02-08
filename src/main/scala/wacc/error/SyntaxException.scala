package wacc.error

import parsley.Failure

final case class SyntaxException(private val message: String = "", private val cause: Throwable = None.orNull) extends CompilerException(message, cause, 100)

object SyntaxException {
    def apply(failure: Failure[_]): SyntaxException = return new SyntaxException(failure.toString, null)
    def apply(msg: String): SyntaxException = return new SyntaxException(msg, null)
}