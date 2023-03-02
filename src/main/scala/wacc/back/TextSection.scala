package wacc
package back

import scala.collection.mutable.{Map => MapM}

class TextSection extends DataSection {

    private val table = MapM[String, String]()
    private var counter = 0

    def add(value: String): String = {
        val label = ".L.str" + counter
        table(label) = value.replace("\"","\\\"")
        counter += 1
        return label
    }

    def get(id: String): String = table.get(id) match {
        case Some(x) => x
        case None => ???
    }

    override def toAssembly(): Seq[Instruction] = {
        if (table.isEmpty) return Seq()

        def entryToAssembly(entry: (String, String)): Seq[Instruction] = {
            val label = entry._1
            val data = entry._2

            return Seq(
                Directive(s".word ${data.length}"),
                Label(label),
                Directive(s".asciz \"${data}\"")
            )
        }

        return Section(".data") +: table.map(entryToAssembly).fold(Seq())(_ ++ _) :+ Section(".text")

    }
}