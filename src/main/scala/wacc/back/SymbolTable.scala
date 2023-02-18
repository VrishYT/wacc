package wacc.back

import scala.collection.mutable.{Map => MapM}
import wacc.AST._

class SymbolTable(val data: DataSection) {

    private val table = MapM[String, (Type, Option[String])]()

    def add(id: String, t: Type): Boolean = add(id, t, None)
    def add(id: String, t: Type, addr: String): Boolean = add(id, t, Some(addr))
    def add(id: String, t: Type, addr: Option[String]): Boolean = {
        if (table.contains(id)) {
            // renaming 
            return false
        } else {
            table(id) = (t, addr)
            return true
        }
    }

    def addData(id: String): String = {
        val addr = data.add(id)
        if (!add(id, StringType, addr)){
            ???
        }
        addr
    }

    def get(id: String): Option[(Type, Option[String])] = table.get(id)

    def remove(id: String): Option[(Type, Option[String])] = table.remove(id)
}
