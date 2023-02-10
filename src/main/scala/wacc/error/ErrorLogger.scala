package wacc.error

import parsley.Failure
import parsley.position._
import wacc.AST._
import Errors._

object ErrorLogger {

    def err(failure: Failure[WACCError]) = throw SyntaxException(failure)

    def err(msg: String, exit: Int) = exit match {
        case 100 => throw SyntaxException(msg)
        case 200 => throw TypeException(msg)
        case x => {
            System.err.println(msg)
            sys.exit(x)
        }
    }
    def err(msg: String, pos: (Int, Int)*) = throw TypeException(msg, None, Seq(), pos)
    def err(msg: String, actual: Type, expected: Type, pos: (Int, Int)*) = throw TypeException(msg, Some(actual), Seq(expected), pos)
    def err(msg: String, actual: Type, expected: Seq[Type], pos: (Int, Int)*) = throw TypeException(msg, Some(actual), expected, pos)

}