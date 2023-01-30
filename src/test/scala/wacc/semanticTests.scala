package wacc


import scala.io.Source
import scala.jdk.CollectionConverters._

import org.scalatest.Assertions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Ignore
import org.scalatest.Tag

import java.nio.file.{Files, Paths, Path}

object Testing {

    def getExpectedExitCode(path: Path): Int = {
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

}

@Ignore
class ValidTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/valid")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        test(path.getFileName.toString.replace(".wacc", "") + " are valid wacc files") {
            val expectedExitCode = Testing.getExpectedExitCode(path)
            assert(expectedExitCode != 100 && expectedExitCode != 200)

            val exitCode = Compiler(path.getFileName.toAbsolutePath.toString).compile
            assert(expectedExitCode == exitCode)
        }
    })

}

@Ignore
class SemanticErrorTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/invalid/semanticErr")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        test(path.getFileName.toString.replace(".wacc", "") + " are semantically invalid") {
            val expectedExitCode = Testing.getExpectedExitCode(path)
            assert(expectedExitCode == 200)

            val exitCode = Compiler(path.getFileName.toAbsolutePath.toString).compile
            assert(expectedExitCode == exitCode)
        }
    })

}

@Ignore
class SyntacticErrorTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/invalid/syntaxErr")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        test(path.getFileName.toString.replace(".wacc", "") + " have invalid syntax") {
            val expectedExitCode = Testing.getExpectedExitCode(path)
            assert(expectedExitCode == 100)

            val exitCode = Compiler(path.getFileName.toAbsolutePath.toString).compile
            assert(expectedExitCode == exitCode)
        }
    })

}