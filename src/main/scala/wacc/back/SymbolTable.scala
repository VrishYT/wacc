package wacc.back

import scala.collection.mutable.{Set, Map => MapM}
import wacc.ast._

class SymbolTable(val text: TextSection = new TextSection, val pre: Set[DataSection] = Set(), val post: Set[DataSection] = Set()) {

    private val table = MapM[String, (Type, Option[String])]()
    private var labelCount = 0

    def add(id: String, t: Type): Boolean = add(id, t, None)
    def add(id: String, t: Type, addr: String): Boolean = add(id, t, Some(addr))
    def add(id: String, t: Type, addr: Option[String]): Boolean = {
        if (table.contains(id)) {
            if (table.get(id) == t) {
                table(id) = (t, addr)
                return true
            }
            return false
        } else {
            table(id) = (t, addr)
            return true
        }
    }

    def addData(id: String): String = {
        val addr = text.add(id)
        if (!add(id, StringType, addr)){
            ???
        }
        addr
    }

    def getAddress(id: String) : String = {
        this.get(id) match {
            case Some((t, addr)) => addr match {
                case Some(address) => address
                case None => ???
            }
            case None => ???
        }
    }

    def getType(id: String) : Type = {
        this.get(id) match {
            case Some((t, addr)) => t
            case None => ???
        }
    }

    def get(id: String): Option[(Type, Option[String])] = table.get(id)

    def remove(id: String): Option[(Type, Option[String])] = table.remove(id)

    def generateLabel: String = {
        val l = ".L" + labelCount
        labelCount += 1
        return l
    }
}
