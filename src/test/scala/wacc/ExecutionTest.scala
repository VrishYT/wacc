package wacc

import org.scalatest._

import scala.sys.process._
import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Paths, Path}
import java.io.{File, BufferedReader, FileReader}
import scala.jdk.StreamConverters._
import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer

@Ignore
class ExecutionTest extends AnyFunSuite {

    def getOutputAndExit(path: Path): (Int, Seq[String]) = {
        val iterator = new BufferedReader(new FileReader(path.toFile.getAbsolutePath)).lines().toScala(Iterator);

        def getOutput: ArrayBuffer[String] = {
            val output = ArrayBuffer[String]()
            while (iterator.hasNext) {
                val line = iterator.next().trim
                if (line.startsWith("#")) {
                    output += line.substring(1).trim
                } else return output
            }
            return output // unreachable 
        }

        var output = Seq[String]()

        while (iterator.hasNext) {
            val line = iterator.next().trim
            if (line contains "# Output:") {
                output = getOutput.toSeq
            }
            else if (line contains "# Exit:") {
                return (iterator.next().substring(1).trim.toInt, output)
            } else if (line contains "# Program:") {
                return (0, output)
            }
        }
        return (0, Seq()) // unreachable

    }

    val make = "make".!!
    var examples = Paths.get("src/test/scala/wacc/wacc_examples/valid/")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString.endsWith(".wacc")).foreach(path => {
        val filename = path.getFileName.toString.replace(".wacc", "")
        val parentPath = path.getParent.toString
        val parent = parentPath.substring(parentPath.lastIndexOf("valid/") + 6) + "/"
        test(parent + filename + " executed as expected") {
            val expected = getOutputAndExit(path)

            val compilation = Seq("./compile", path.toString).!!
            val basename = path.toString.replace(".wacc", "")
            val gcc = Seq("arm-linux-gnueabi-gcc", "-o", basename, "-mcpu=arm1176jzf-s", "-mtune=arm1176jzf-s", basename + ".s").!!

            val out = new StringBuilder
            val err = new StringBuilder
            val logger = ProcessLogger(stdout append _, stderr append _)

            val exit = Seq("qemu-arm", "-L", "/usr/arm-linux-gnueabi/", basename).!(logger)

            assert(exit == expected._1)
            assert(out == expected._2.mkString("\n"))

        }
    })

}