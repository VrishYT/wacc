package wacc

import scala.collection.mutable.{Set, Map => MapM}
import scala.collection.immutable.Map
import back._
import ast._

sealed trait TableEntry 
sealed abstract class Table extends TableEntry {

    private var ifCount = 0
    private var whileCount = 0
    private var beginCount = 0 
    private var size = 0

    def isInFunction: Boolean
    def getReturnType: Type

    private val table = MapM[String, TableEntry]()

    def getSize: Int = size

    def getType(id: String): Type = getSymbol(id) match {
        case Some(x) => x.t
        case None => ???
    }

    private def update(id: String, symbol: Symbol): Unit = {
        table(id) = symbol
    }
    
    // def update(id: String, reg: Register): Unit = {
    //     table(id) = RegSymbol(getType(id), reg)
    // }

    // def update(id: String, addr: Address): Unit = {
    //     table(id) = MemSymbol(getType(id), addr)
    // }
    
    // def update(id: String, label: String): Unit = {
    //     table(id) = LabelSymbol(getType(id), label)
    // }

    def update(id: String, op: Operand): Unit = {
        table(id) = OpSymbol(getType(id), op)
    }

    // USED FOR FRONT-END
    def add(id: String, symbol: Symbol): Boolean = {
        getSymbol(id) match {
            case Some(x) => x match {
                case _: ParamSymbol => 
                case _ => return false 
            }
            case None => 
        }
        table(id) = symbol
        size += 1
        return true
    }

    private def addTable(id: String, vars: Table) = {
        table(id) = vars
    }

    def addIf(thenVars: ChildTable, elseVars: ChildTable) = {
        addTable(s"_if${ifCount}", thenVars)
        addTable(s"_else${ifCount}", elseVars)
        ifCount += 1
    }

    def addWhile(vars: ChildTable) = {
        addTable(s"_while${whileCount}", vars)
        whileCount += 1
    }

    def addBegin(vars: ChildTable) = {
        addTable(s"_begin${beginCount}", vars)
        beginCount += 1
    }

    def contains(id: String): Boolean = table.contains(id)

    def getSymbol(id: String): Option[Symbol] = table.get(id) match {
        case x: Some[Symbol] => x
        case None => None
    }

    def getTable(id: String): Option[ChildTable] = table.get(id) match {
        case x: Some[ChildTable] => x
        case None => None
    }

    def isEmpty: Boolean = table.isEmpty

}

case class FuncTable(val id: String, val paramTypes: Seq[Type], val returnType: Type) extends Table {
    override def isInFunction = id != "main"
    override def getReturnType = returnType
}
case class ChildTable(val parent: Table) extends Table {
    override def isInFunction = parent.isInFunction
    override def getReturnType = parent.getReturnType
}

sealed class Symbol(val t: Type) extends TableEntry
object Symbol {
    def apply(t: Type): Symbol = new Symbol(t)
}

case class ParamSymbol(override val t: Type) extends Symbol(t)
case class OpSymbol(override val t: Type, val op: Operand) extends Symbol(t)
// case class RegSymbol(override val t: Type, val reg: Register) extends Symbol(t)
// case class MemSymbol(override val t: Type, val addr: Address) extends Symbol(t)
// case class LabelSymbol(override val t: Type, val label: String) extends Symbol(t)

class SymbolTable {

    private val table = MapM[String, FuncTable]()

    def declare(id: String): FuncTable = declare(id, Seq(), AnyType)
    def declare(id: String, params: Seq[Param], returnType: Type): FuncTable = {
        val func = new FuncTable(id, params.map(_.t), returnType)
        table(id) = func
        params.foreach(param => func.add(param.id, ParamSymbol(param.t)))
        return func
    }

    def get(id: String): Option[FuncTable] = table.get(id)

    def contains(id: String): Boolean = table.contains(id)

}