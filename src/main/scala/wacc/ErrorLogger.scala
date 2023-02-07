package wacc

import parsley.Failure

sealed class CompilerException(private val message: String = "", private val cause: Throwable = None.orNull, private val exit: Int = -1) extends Exception(message, cause) {
    override def toString(): String = message
    def quit: Unit = {
        System.err.println(message)
        // sys.exit(exit)
    }
}

object CompilerException {
    def apply(msg: String, exit: Int): CompilerException = new CompilerException(msg, null, exit)
}

final case class TypeException(private val message: String = "", private val cause: Throwable = None.orNull) extends CompilerException(message, cause, 200)

object TypeException {
    def apply(msg: String): TypeException = return TypeException(msg, null)
    def apply(msg: String, cause: Throwable): TypeException = return new TypeException(msg, cause)
}

final case class SyntaxException(private val message: String = "", private val cause: Throwable = None.orNull) extends CompilerException(message, cause, 100)

object SyntaxException {
    def apply(failure: Failure[_]): SyntaxException = return new SyntaxException(failure.toString, null)
    def apply(msg: String): SyntaxException = return new SyntaxException(msg, null)
}

object ErrorLogger {

    def err(failure: Failure[_]) = throw SyntaxException(failure)

    def err(msg: String) = throw TypeException(msg)

    def err(msg: String, exit: Int) = exit match {
        case 100 => throw SyntaxException(msg)
        case 200 => throw TypeException(msg)
        case x => throw CompilerException(msg, exit)
    }

}