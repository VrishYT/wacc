package wacc.error

import parsley.Failure
import parsley.position._
import wacc.AST._
import Errors._

object ErrorLogger {

    def err(msg: String, exit: Int) = {
        System.err.println(msg)
        sys.exit(exit)
    }

    def err(msg: String, pos: (Int, Int)*) = throw new TypeException(msg, None, pos)
    def err(msg: String, actual: Type, expected: Type, pos: (Int, Int)*) = throw new TypeException(msg, Some(actual, Seq(expected)), pos)
    def err(msg: String, actual: Type, expected: Seq[Type], pos: (Int, Int)*) = throw new TypeException(msg, Some(actual, expected), pos)

}