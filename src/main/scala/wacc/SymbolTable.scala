package wacc

import scala.collection.mutable.{Map => MapM}
import back._
import ast._

sealed trait TableEntry 
sealed abstract class Table extends TableEntry {

    var ifCount = 0
    var whileCount = 0
    var beginCount = 0 
    private var size = 0

    def isInFunction: Boolean
    def getReturnType: Type
    def setReturnType(t: Type): Unit

    val table = MapM[String, TableEntry]()

    override def toString(): String = "\n" + table.filter(x => x._2.isInstanceOf[OpSymbol] || x._2.isInstanceOf[Table]).mkString("\n") + "\n ----"

    def getSize: Int = size

    def resetCounts(): Unit = {
        ifCount = 0
        whileCount = 0
        beginCount = 0
    }

    private def updateRecursive(id: String, symbol: Symbol): Unit = {

        def updateParent(id: String, symbol: Symbol, table: Table): Unit = {
            if (table.contains(id)) table.table(id) = symbol
            else {
                table match {
                    case ChildTable(parent) => updateParent(id, symbol, parent)
                    case _ => ??? 
                }
            }
        }

        updateParent(id, symbol, this)
    }

    def getIDFromReg(reg: Register): Option[String] = {
        
        def getFromParent(table: Table): Option[String] = {
            val filtered = table.table.filter(_._2 match {
                case OpSymbol(_, op) => op == reg
                case _ => false
            })
            if (filtered.isEmpty) {
                table match {
                    case ChildTable(parent) => getFromParent(parent)
                    case _ => None
                }
            } else if (filtered.size > 1) None
            else Some(filtered.head._1)
        }

        getFromParent(this)
    }

    def update(id: String, op: Operand): Unit = {
        val t = getType(id) match {
            case Some(x) => x
            case None => ???
        }
        updateRecursive(id, OpSymbol(t, op))
    }

    def updateEntry(id: String, reg: Register, op: Operand): Unit = {

        def update(table: Table): Unit = {
            // println(s"$id, $reg in $table")
            val filtered = table.table.filter(entry => {
                val isReg: Boolean = entry._2 match {
                    case OpSymbol(_, op) => op match {
                        case x: Register => reg.i == x.i
                        case _ => false
                    }
                    case _ => false
                }
                (entry._1 == id) && isReg
            })
            if (filtered.isEmpty) {
                table match {
                    case ChildTable(parent) => update(parent)
                    case _ => ???
                }
            } else if (filtered.size > 1) ???
            else {
                table.update(id, op)
                // println(s"$id, $reg\nupdated $table")
            }
        }

        update(this)

    }

    def add(id: String, symbol: Symbol): Boolean = {
        table.get(id) match {
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

    private def get(id: String): Option[TableEntry] = {

        def getFromParent(id: String, table: Table): Option[TableEntry] = table.table.get(id) match {
            case x: Some[_] => x
            case None => table match {
                case ChildTable(parent) => getFromParent(id, parent)
                case _ => None
            }
        }

        getFromParent(id, this)
    }

    def getType(id: String): Option[Type] = getSymbol(id) match {
        case Some(x) => Some(x.t)
        case _ => None
    }

    def getOp(id: String): Operand = getSymbol(id) match {
        case Some(x) => x match {
            case x: OpSymbol => x.op
            case _ => this match {
                case ChildTable(parent) => parent.getOp(id)
            case _ => NoOperand(id)
            }
        }
        case _ => NoOperand(id)
    }

    private def getSymbol(id: String): Option[Symbol] = get(id) match {
        case Some(x) => x match {
            case x: Symbol => Some(x)
            case _ => None 
        }
        case None => None
    }

    def getTable(id: String): Option[ChildTable] = get(id) match {
        case Some(x) => x match {
            case x: ChildTable => Some(x)
            case _ => None 
        }
        case None => None
    }

    def isEmpty: Boolean = table.isEmpty

}

case class FuncTable(val id: String, val paramTypes: Seq[Type], var returnType: Type) extends Table {
    override def isInFunction = id != "main"
    override def getReturnType = returnType
    override def setReturnType(t: Type): Unit = {
        returnType = t
    }
}
case class ChildTable(val parent: Table) extends Table {
    override def isInFunction = parent.isInFunction
    override def getReturnType = parent.getReturnType
    override def setReturnType(t: Type): Unit = {
        
    }
}

sealed class Symbol(val t: Type) extends TableEntry
object Symbol {
    def apply(t: Type): Symbol = new Symbol(t)
}

case class ParamSymbol(override val t: Type) extends Symbol(t)
case class OpSymbol(override val t: Type, val op: Operand) extends Symbol(t)

class SymbolTable {

    private val table = MapM[String, FuncTable]()

    override def toString(): String = table.mkString("\n")

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