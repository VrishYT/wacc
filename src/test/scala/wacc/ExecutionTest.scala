package wacc

import org.scalatest._

import scala.sys.process._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Span, Seconds}
import java.nio.file.{Files, Paths, Path}
import java.io.{File, BufferedReader, FileReader}
import scala.jdk.StreamConverters._
import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.concurrent._
import ExecutionContext.Implicits.global

// @Ignore
class ExecutionTest extends AnyFunSuite with BeforeAndAfter with TimeLimitedTests {

    val timeLimit = Span(5, Seconds)

    def getIO(path: Path): Option[(Int, Seq[String], Seq[String])] = {
        val iterator = new BufferedReader(new FileReader(path.toFile.getAbsolutePath)).lines().toScala(Iterator);

        def getOutput(output: ListBuffer[String]): Unit = {
            while (iterator.hasNext) {
                val line = iterator.next().trim
                if (line.startsWith("#")) {
                    val sanitised = line.substring(1).trim
                    if (sanitised.isEmpty) return
                    else output += line.substring(1).trim
                } else return
            }
            return // unreachable 
        }

        val input = ListBuffer[String]()
        val output = ListBuffer[String]()

        while (iterator.hasNext) {
            val line = iterator.next().trim
            if (line contains "NO_EXEC") {
                return None
            }
            else if (line contains "# Input:") {
                input ++= line.replace("# Input: ", "").split(" ")
            }
            else if (line contains "# Output:") getOutput(output)
            else if (line contains "# Exit:") {
                return Some((iterator.next().substring(1).trim.toInt, input.toSeq, output.toSeq))
            } else if (line contains "# Program:") {
                return Some((0, input.toSeq, output.toSeq))
            }
        }
        return Some((0, Seq(), Seq())) // unreachable

    }

    def testFile(path: Path) {
        val filename = path.getFileName.toString.replace(".wacc", "")
        val parentPath = path.getParent.toString
        val parent = parentPath.substring(parentPath.lastIndexOf("valid/") + 6) + "/"

        getIO(path) match {
            case Some(expected) => {
                test(parent + filename + " executed as expected") {

                    val compilation = Seq("./compile", path.toString).!!
                    val basename = path.getFileName.toString.replace(".wacc", "")
                    val gcc = Seq("arm-linux-gnueabi-gcc", "-o", basename, "-mcpu=arm1176jzf-s", "-mtune=arm1176jzf-s", basename + ".s").!!

                    val exec = Seq("qemu-arm", "-L", "/usr/arm-linux-gnueabi/", basename)

                    val out = ListBuffer[String]()
                    var err = ListBuffer[String]()

                    val p = exec.run(new ProcessIO(
                        in => {
                            expected._3.foreach(input => in.write(input.getBytes))
                            in.close
                        }, 
                        out ++= Source.fromInputStream(_).getLines, 
                        err ++= Source.fromInputStream(_).getLines
                    ))

                    try {
                        Await.result(Future(blocking(p.exitValue)), duration.Duration(5, "sec"))
                    } catch {
                        case _: TimeoutException => {
                            Seq("rm", basename).!!
                            Seq("rm", basename + ".s").!!
                            fail("TIMEOUT")
                        }
                    }

                    val exit = p.exitValue

                    Seq("rm", basename).!!
                    Seq("rm", basename + ".s").!!

                    assert(exit == expected._1)
                    assert(out.map(_.replaceAll("0[xX][0-9a-fA-F]+", "#addrs#")).mkString == expected._3.mkString)

                }
            }
            case None => println(s"ignored file ${path.getFileName()}")
        }


    }


    val make = "make".!!
    var examples = Paths.get("src/test/scala/wacc/wacc_examples/valid")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString.endsWith(".wacc")).foreach(testFile(_))

}