package wacc.back

import scala.collection.mutable.{Map => MapM}

class SymbolTable {

    private val table = MapM[String, (Type, Option[String])]()

    def add(id: String, t: Type) = add(id, t, None)
    def add(id: String, t: Type, addr: String) = {
        if (table.contains(id)) {
            // renaming 
        } else {
            table(id) = (t, Some(addr))
        }
    }

    def get(id: String): Option[(Type, Option[String])] = table.get(id)

    def remove(id: String): Option[(Type, Option[String])] = table.remove(id)
}
