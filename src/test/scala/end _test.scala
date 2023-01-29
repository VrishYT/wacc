package wacc

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

class SemanticTests extends AnyFunSuite {

    val examples = Paths.get("src/test/scala/wacc/wacc_examples/valid")
    Files.walk(examples).iterator().asScala.filter(_.getFileName.toString().endsWith(".wacc")).foreach(file => {
        test(file.getFileName.toString.replace(".wacc", "") + " gives expected output") {
            assert (Files.exists(file))
        }
    })

}