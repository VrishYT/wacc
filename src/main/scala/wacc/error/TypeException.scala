package wacc
package error

import wacc.AST._
import scala.collection.mutable.{Map => MapM}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.StreamConverters._

import java.io.File
import java.io.{BufferedReader, FileReader}

/* TypeException class which is thrown when errors from SemanticChecker are found */
class TypeException(private val message: String, 
        private val types: Option[(Type, Seq[Type])] = None,
        private val pos: Seq[(Int, Int)]) extends Exception(message) {

    /* Convert TypeException into a string with the error info */
    override def toString: String = message + (types match {
        case Some(x) => "\n" + 
        "  - actual type     : " + x._1 + "\n" + 
        "  - expected type(s): " + x._2.mkString(",")
        case _ => ""
    })
}

/* Companion objet with generic methods related to TypeException's */
object TypeException {

    /* Function used to convert all caught TypeExceptions into WACCErrors */
    def convertErrors(errors: ArrayBuffer[TypeException], file: File): ArrayBuffer[WACCError] = {
        val table = MapM[Int, String]()
        val iterator = new BufferedReader(new FileReader(file.getAbsolutePath)).lines().toScala(Iterator);
        var linesRead: Int = 0

        /* Gets line from file. Errors with exit code 1 if EOF reached */
        def getIteratorLine(i: Int): String = if (iterator.hasNext) {
            var l: String = ""
            while (linesRead < i && iterator.hasNext) {
                l = iterator.next
                linesRead += 1
            }
            return l.replaceAll("\t", "    ")
        } else ErrorLogger.err("End of input reached", 1)

        /* Tabulates file line data to prevent repeated access to lines that are used multiple times.
           Useful for files that have semantic errors on multiple lines sequentially. 
           Overlapping "code previews" in the error message can use tabulate lines rather than repeated file access. */
        def getLine(i: Int): String = {
            // could be replaced with getOrElseUpdate(i)
            table.get(i) match {
                case Some(x) => return x
                case None => {
                    val line = getIteratorLine(i)
                    table(i) = line
                    return line
                }
            }
        }

        /* Generates an ArrrayBuffer of WACCErrors using data held in each TypeException */        
        return errors.map(err => {
            val row = err.pos(0)._1
            val start: Int = (row - error.NUM_LINES_BEFORE).max(1)
            val end: Int = row + error.NUM_LINES_AFTER
            val code = ArrayBuffer[String]()
            for (line <- start to end) code += getLine(line)

            WACCError(err.pos, Some(file.getName), SemanticError(err.toString + "\n", TypecheckErrorInfo(code.toSeq)))
        })

    }
}