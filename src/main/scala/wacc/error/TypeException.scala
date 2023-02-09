package wacc.error

import wacc.AST._

final case class TypeException(private val message: String, 
        private val actual: Option[Type], 
        private val expected: Seq[Type], 
        private val pos: Seq[(Int, Int)])
    extends CompilerException(message, 200) {
    override def toString: String = {
        "pos: " + pos + "\n" + (actual match {
            case Some(x) => "actual type: " + x + "\n" + "expected type(s): <" + expected.mkString(",") + ">\n"
            case _ => ""
        }) + message
    }
}

object TypeException {
    def apply(msg: String): TypeException = TypeException(msg, None, Seq(), Seq())
    def apply(msg: String, pos: Seq[(Int, Int)]): TypeException = TypeException(msg, None, Seq(), pos)
    def apply(msg: String, actual: Type, expected: Type, pos: Seq[(Int, Int)]): TypeException = TypeException(msg, Some(actual), Seq(expected), pos)
    def apply(msg: String, actual: Type, expected: Seq[Type], pos: Seq[(Int, Int)]): TypeException = TypeException(msg, Some(actual), expected, pos)
}