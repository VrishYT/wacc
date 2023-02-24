package wacc

import scala.collection.mutable.{Set, Map => MapM}
import back._
import ast._

sealed trait TableEntry 

class GlobalTable extends TableEntry {
    val table = MapM[String, ChildTable]()
}

class ChildTable(val parent: TableEntry) extends TableEntry {

    val table = MapM[String, TableEntry]()

    def add(id: String, child: ChildTable): Unit = {
        table(id) = child
    } 

    private def get(id: String): TableEntry = table.get(id) match {
        case Some(x) => x
        case _ => ???
    }

    def getSymbol(id: String): Symbol = get(id) match {
        case x: Symbol => x
        case _ => ???
    }

    def getTable(id: String): ChildTable = get(id) match {
        case x: ChildTable => x
        case _ => ???
    }

}

sealed class Symbol(val t: Type) extends TableEntry
case class RegSymbol(override val t: Type, val reg: Register) extends Symbol(t)
case class MemSymbol(override val t: Type, val addr: Int) extends Symbol(t)
case class LabelSymbol(override val t: Type, val label: String) extends Symbol(t)

class SymbolTable {

    private val table = new GlobalTable()

    def declare(id: String): ChildTable = {
        val child = new ChildTable(table)
        table.table(id) = child
        return child
    }

    def get(id: String): ChildTable = {
        table.table.get(id) match {
            case Some(x) => x
            case _ => ???
        }
    }

}