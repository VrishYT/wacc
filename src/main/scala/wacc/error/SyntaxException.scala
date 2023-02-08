package wacc.error

import parsley.Failure

final case class SyntaxException(private val message: String = "") extends CompilerException(message, 100)

object SyntaxException {
    def apply(failure: Failure[_]): SyntaxException = return new SyntaxException(failure.toString)
    def apply(msg: String): SyntaxException = return new SyntaxException(msg)
}