package wacc
package error

import wacc.AST._
import Errors._
import scala.collection.mutable.{Map => MapM}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.StreamConverters._

// import scala.io.Source
import java.io.File
import java.io.{BufferedReader, FileReader}

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

    // TODO: add function declaration line for invalid return type
    def convertErrors(errors: ArrayBuffer[TypeException], file: File): ArrayBuffer[WACCError] = {
        val table = MapM[Int, String]()
        // val iterator = Source.fromFile(file).getLines()
        val iterator = new BufferedReader(new FileReader(file.getAbsolutePath)).lines().toScala(Iterator);
        var linesRead: Int = 0

        // def getReaderLine(i: Int): String = {
        //     val skip = i - linesRead - 1
        //     reader.skip(skip) 
        // }

        def getIteratorLine(i: Int): String = if (iterator.hasNext) {
            var l: String = ""
            while (linesRead < i && iterator.hasNext) {
                l = iterator.next.replaceAll("\t", "    ")
                linesRead += 1
            }
+
            return l
        } else ErrorLogger.err("End of file reached", 1)

        def getLine(i: Int): String = {
            // TODO: replace with getOrElseUpdate(i)
            table.get(i) match {
                case Some(x) => return x
                // case None => getIteratorLine(i)
                case None => getIteratorLine(i)
            }
        }

        return errors.map(err => {
            val row = err.pos(0)._1
            val start: Int = (row - error.NUM_LINES_BEFORE).max(1)
            val end: Int = row + error.NUM_LINES_AFTER
            val code = ArrayBuffer[String]()
            for (line <- start to end) {
                code += getLine(line)
            }
            // err.pos: Seq[(Int,Int)], code: ArrayBuffer[String], err.toString: String, source = file.getName
            // pos used in error = pos.head
            new WACCError(err.pos, Some(file.getName), new SemanticError(err.toString + "\n", new TypecheckErrorInfo(code.toSeq)))
        })

    }

    def apply(msg: String): TypeException = TypeException(msg, None, Seq(), Seq())
    def apply(msg: String, pos: Seq[(Int, Int)]): TypeException = TypeException(msg, None, Seq(), pos)
    def apply(msg: String, pos: (Int, Int)): TypeException = TypeException(msg, None, Seq(), Seq(pos))
    def apply(msg: String, actual: Type, expected: Type, pos: (Int, Int)): TypeException = TypeException(msg, Some(actual), Seq(expected), Seq(pos))
    def apply(msg: String, actual: Type, expected: Type, pos: Seq[(Int, Int)]): TypeException = TypeException(msg, Some(actual), Seq(expected), pos)
    def apply(msg: String, actual: Type, expected: Seq[Type], pos: (Int, Int)): TypeException = TypeException(msg, Some(actual), expected, Seq(pos))
    def apply(msg: String, actual: Type, expected: Seq[Type], pos: Seq[(Int, Int)]): TypeException = TypeException(msg, Some(actual), expected, pos)
}