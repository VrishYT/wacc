package wacc

import scala.jdk.CollectionConverters._
import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Paths}

class ValidTypecheckerTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/valid")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString.endsWith(".wacc")).foreach(path => {
        val filename = path.getFileName.toString.replace(".wacc", "")
        val parentPath = path.getParent.toString
        val parent = parentPath.substring(parentPath.lastIndexOf("valid/") + 6) + "/"
        test(parent + filename + " are valid wacc files") {
            val c = Compiler(path.toString)

            val parseSuccess: Boolean = c.parse()
            assert(parseSuccess)

            val semanticSuccess: Boolean = c.typecheck
            assert(semanticSuccess, "\n" + c.toString) 
        }
    })

}

class InvalidTypecheckerTests extends AnyFunSuite {

    val subdir = "semanticErr/"
    val examples = Paths.get("src/test/scala/wacc/wacc_examples/invalid/" + subdir)
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString.endsWith(".wacc")).foreach(path => {
        val filename = path.getFileName.toString.replace(".wacc", "")
        val parentPath = path.getParent.toString
        val parent = parentPath.substring(parentPath.lastIndexOf(subdir) + subdir.length) + "/"
        test(parent + filename + " are semantically invalid") {
            val c = Compiler(path.toString)

            val parseSuccess: Boolean = c.parse()
            assert(parseSuccess)
            
            val typecheckSuccess = c.typecheck
            assert(!typecheckSuccess)
        }
    })

}