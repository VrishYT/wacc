package wacc


import scala.io.Source
import scala.jdk.CollectionConverters._

import org.scalatest.Assertions._
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths, Path}

class SemanticTests extends AnyFunSuite {

    private def getExitCode(path: Path): Int = {
        var foundExit = false
        for (line <- Source.fromFile(path.toString).getLines) {

            if (foundExit == true) {
                return line.substring(2).toInt
            }

            if (line == "# Exit:") {
                foundExit = true
            }
        }
        return 0
    }

    val examples = Paths.get("src/test/scala/wacc/wacc_examples")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(file => {
        test(file.getFileName.toString.replace(".wacc", "") + " gives expected output") {
            val absPath = file.toAbsolutePath.toString
            val exitCode = getExitCode(file)

            exitCode match {
                case 200 => assert(absPath.contains("/invalid/") && absPath.contains("/semanticErr/"))
                case 100 => assert(absPath.contains("/invalid/") && absPath.contains("/syntaxErr/"))
                case default => assert(absPath.contains("/valid/"))
            }
        }
    })


}