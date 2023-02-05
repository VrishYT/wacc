package wacc


import scala.io.Source
import scala.jdk.CollectionConverters._

import org.scalatest.Assertions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Ignore
import org.scalatest.Tag

import java.nio.file.{Files, Paths, Path}

class ValidTypecheckerTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/valid")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        test(path.getFileName.toString.replace(".wacc", "") + " are valid wacc files") {
            val c = Compiler(path.toString)
            
            val readSuccess = c.readTarget
            assert(readSuccess)

            val parseSuccess: Boolean = c.parse
            assert(parseSuccess)

            val semanticSuccess: Boolean = c.typecheck
            assert(semanticSuccess, "\n" + c.toString) 
        }
    })

}

class InvalidTypecheckerTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/invalid/semanticErr")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        test(path.getFileName.toString.replace(".wacc", "") + " are semantically invalid") {
            val c = Compiler(path.toString)
            
            val readSuccess = c.readTarget
            assert(readSuccess)

            val parseSuccess: Boolean = c.parse
            assert(parseSuccess)

            val semanticSuccess: Boolean = c.typecheck
            assert(!semanticSuccess, "\n" + c.toString) 
        }
    })

}