package wacc


import scala.io.Source
import scala.jdk.CollectionConverters._

import org.scalatest.Assertions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Ignore

import java.nio.file.{Files, Paths}

import error._

class ValidParseTests extends AnyFunSuite {

    var examples = Paths.get("src/test/scala/wacc/wacc_examples/valid")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        val filename = path.getFileName.toString.replace(".wacc", "")
        val parentPath = path.getParent.toString
        val parent = parentPath.substring(parentPath.lastIndexOf("valid/") + 6) + "/"
        test(parent + filename + " has valid syntax") {
            val c = Compiler(path.toString)

            val parseSuccess: Boolean = c.parse
            assert(parseSuccess, "\n" + c.toString)
        }
    })

    val subdir = "semanticErr/"
    examples = Paths.get("src/test/scala/wacc/wacc_examples/invalid/" + subdir)
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        val filename = path.getFileName.toString.replace(".wacc", "")
        val parentPath = path.getParent.toString
        val parent = parentPath.substring(parentPath.lastIndexOf(subdir) + subdir.length) + "/"
        test(subdir + parent + filename + " has valid syntax") {
            val c = Compiler(path.toString)

            val parseSuccess: Boolean = c.parse
            assert(parseSuccess, "\n" + c.toString)
        }
    })

}

class InvalidParseTests extends AnyFunSuite {

    val subdir = "syntaxErr/"
    val examples = Paths.get("src/test/scala/wacc/wacc_examples/invalid/" + subdir)
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(path => {
        val filename = path.getFileName.toString.replace(".wacc", "")
        val parentPath = path.getParent.toString
        val parent = parentPath.substring(parentPath.lastIndexOf(subdir) + subdir.length) + "/"
        test(parent + filename + " has invalid syntax") {
            val c = Compiler(path.toString)

            val parseSuccess: Boolean = c.parse
            assert(!parseSuccess, "\n" + c.toString)
        }
    })

}