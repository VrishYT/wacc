package wacc.error

import parsley.Failure
import Errors._

final case class SyntaxException(private val message: String = "") extends CompilerException(message, 100)

object SyntaxException {
    def apply(failure: Failure[WACCError]): SyntaxException = return new SyntaxException(failure.msg.toString)
}