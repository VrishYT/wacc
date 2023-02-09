package wacc.error

import parsley.Failure
import parsley.position._
import wacc.AST._

object ErrorLogger {

    def err(failure: Failure[_]) = throw SyntaxException(failure)

    def err(msg: String) = throw TypeException(msg, None, Seq(), Seq())
    def err(msg: String, pos: (Int, Int)*) = throw TypeException(msg, None, Seq(), pos)
    def err(msg: String, actual: Type, expected: Type, pos: (Int, Int)*) = throw TypeException(msg, Some(actual), Seq(expected), pos)
    def err(msg: String, actual: Type, expected: Seq[Type], pos: (Int, Int)*) = throw TypeException(msg, Some(actual), expected, pos)

    def err(msg: String, exit: Int) = exit match {
        case 100 => throw SyntaxException(msg)
        case 200 => throw TypeException(msg)
        case x => throw CompilerException(msg, exit)
    }

}