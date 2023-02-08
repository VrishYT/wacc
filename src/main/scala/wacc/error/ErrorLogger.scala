package wacc.error

import parsley.Failure

object ErrorLogger {

    def err(failure: Failure[_]) = throw SyntaxException(failure)

    def err(msg: String) = throw TypeException(msg)

    def err(msg: String, exit: Int) = exit match {
        case 100 => throw SyntaxException(msg)
        case 200 => throw TypeException(msg)
        case x => throw CompilerException(msg, exit)
    }

}