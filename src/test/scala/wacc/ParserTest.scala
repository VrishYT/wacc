package wacc


import scala.io.Source
import scala.jdk.CollectionConverters._

import org.scalatest.Assertions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Ignore

import java.nio.file.{Files, Paths}

@Ignore
class ValidParseTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/valid")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        test(path.getFileName.toString.replace(".wacc", "") + " are valid wacc files") {
            val success: Boolean = Compiler(path.getFileName.toAbsolutePath.toString).parse
            assert(success)
        }
    })

}

@Ignore
class SemanticErrorParseTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/invalid/semanticErr")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        test(path.getFileName.toString.replace(".wacc", "") + " are semantically invalid") {
            val success: Boolean = Compiler(path.getFileName.toAbsolutePath.toString).parse
            assert(success)
        }
    })

}

@Ignore
class SyntacticErrorParseTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/invalid/syntaxErr")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        test(path.getFileName.toString.replace(".wacc", "") + " have invalid syntax") {
            val success: Boolean = Compiler(path.getFileName.toAbsolutePath.toString).parse
            assert(!success)
        }
    })

}