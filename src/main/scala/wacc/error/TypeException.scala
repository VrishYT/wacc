package wacc
package error

import wacc.AST._
import Errors._
import scala.collection.mutable.{Map => MapM}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.StreamConverters._

import java.io.File
import java.io.{BufferedReader, FileReader}

class TypeException(private val message: String, 
        private val types: Option[(Type, Seq[Type])] = None,
        private val pos: Seq[(Int, Int)]) extends Exception(message) {

    // override def toString: String = {
    //     "pos: " + pos + "\n" + (types match {
    //         case Some(x) => "actual type: " + x._1 + "\n" + "expected type(s): <" + x._2.mkString(",") + ">\n"
    //         case _ => ""
    //     }) + message
    // }
}

object TypeException {

    // TODO: add function declaration line for invalid return type
    def convertErrors(errors: ArrayBuffer[TypeException], file: File): ArrayBuffer[WACCError] = {
        val table = MapM[Int, String]()
        val iterator = new BufferedReader(new FileReader(file.getAbsolutePath)).lines().toScala(Iterator);
        var linesRead: Int = 0

        def getIteratorLine(i: Int): String = if (iterator.hasNext) {
            var l: String = ""
            while (linesRead < i && iterator.hasNext) {
                l = iterator.next
                linesRead += 1
            }
            return l.replaceAll("\t", "    ")
        } else ErrorLogger.err("End of file reached", 1)

        def getLine(i: Int): String = {
            // TODO: replace with getOrElseUpdate(i)
            table.get(i) match {
                case Some(x) => return x
                // case None => getIteratorLine(i)
                case None => {
                    val line = getIteratorLine(i)
                    table(i) = line
                    return line
                }
            }
        }

        return errors.map(err => {
            val row = err.pos(0)._1
            val start: Int = (row - error.NUM_LINES_BEFORE).max(1)
            val end: Int = row + error.NUM_LINES_AFTER
            val code = ArrayBuffer[String]()
            for (line <- start to end) code += getLine(line)

            new WACCError(err.pos, Some(file.getName), new SemanticError(err.toString + "\n", new TypecheckErrorInfo(code.toSeq)))
        })

    }
}