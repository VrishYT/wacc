package wacc.back

import scala.collection.mutable.{Map => MapM}
import wacc.back._

class DataSection {

    // TODO: change key type to label ???
    private val table = MapM[String, String]()
    private var counter = 0

    def add(value: String): String = {
        val label = ".L.str" + counter
        table(label) = value
        counter += 1
        return label
    }

    def get(id: String): String = table.get(id) match {
        case Some(x) => x
        case None => ???
    }

    override def toString(): String = {
        if (table.isEmpty) return ""

        // TODO: update according to other TODO's related to DataSection
        def entryToAssembly(entry: (String, String)): String = {
            val label = entry._1
            val data = entry._2

            return s"\t.word ${data.length()}\n" + 
                   label + ":\n" +
                   s"\t.asciz \"${data}\""
        }

        return ".data\n" + 
               table.toSeq.map(entryToAssembly).mkString +
               ".text\n"
    }
    
}
