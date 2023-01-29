package wacc

import parsley.{Parsley, Success, Failure}
import parsley.character.digit
import parsley.expr.chain
import parsley.implicits.character.charLift
import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
        println("Hello WACC_11!")

        if (args.length == 0) {
            printUsage
            return
        }

        val filename = args(0)
        val fileData = openFile(filename)
        println(fileData) 
        
    }

    def printUsage : Unit = {
        println("Incorrect usage.") // TODO: add usage message
    }

    def openFile(path: String) : String = {
        val builder = new StringBuilder()
        for (line <- Source.fromFile(path).getLines) {
            builder.append(line)
            builder.append("\n")
        }
        return builder.toString()
        
    }

}

