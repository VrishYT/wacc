package wacc


import scala.io.Source
import scala.jdk.CollectionConverters._

import org.scalatest.Assertions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Ignore
import org.scalatest.Tag

import java.nio.file.{Files, Paths, Path}

import error._

class ValidTypecheckerTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/valid")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        val filename = path.getFileName.toString.replace(".wacc", "")
        val parentPath = path.getParent.toString
        val parent = parentPath.substring(parentPath.lastIndexOf("valid/") + 6) + "/"
        test(parent + filename + " are valid wacc files") {
            val c = Compiler(path.toString)

            val parseSuccess: Boolean = c.parse
            assert(parseSuccess)

            val semanticSuccess: Boolean = c.typecheck
            assert(semanticSuccess, "\n" + c.toString) 
        }
    })

}

class InvalidTypecheckerTests extends AnyFunSuite {

    val subdir = "semanticErr/"
    val examples = Paths.get("src/test/scala/wacc/wacc_examples/invalid/" + subdir)
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        val filename = path.getFileName.toString.replace(".wacc", "")
        val parentPath = path.getParent.toString
        val parent = parentPath.substring(parentPath.lastIndexOf(subdir) + subdir.length) + "/"
        test(parent + filename + " are semantically invalid") {
            val c = Compiler(path.toString)

            val parseSuccess: Boolean = c.parse
            assert(parseSuccess)
            
            assertThrows[TypeException] {
                c.typecheck
            }
        }
    })

}