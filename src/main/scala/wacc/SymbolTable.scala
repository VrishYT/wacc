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

    def getReturnType: Type = AnyType

    val table = MapM[String, TableEntry]()

    override def toString(): String = "\n" + table.filter(x => x._2.isInstanceOf[OpSymbol] || x._2.isInstanceOf[Table]).mkString("\n") + "\n ----"

    def getSize: Int = size

    def resetCounts(): Unit = {
        ifCount = 0
        whileCount = 0
        beginCount = 0
    }

    def updateRecursive(id: String, symbol: Symbol): Unit = {

        def updateParent(id: String, symbol: Symbol, table: Table): Unit = {
            if (table.contains(id)) table.table(id) = symbol
            else {
                table match {
                    case x: ChildTable => updateParent(id, symbol, x.parent)
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
                    case x: ChildTable => getFromParent(x.parent)
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
                    case x: ChildTable => update(x.parent)
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

    def addTable(id: String, vars: Table) = {
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

    protected def get(id: String): Option[TableEntry] = {

        def getFromParent(id: String, table: Table): Option[TableEntry] = table.table.get(id) match {
            case x: Some[_] => x
            case None => table match {
                case x: ChildTable => getFromParent(id, x.parent)
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
                case x: ChildTable => x.parent.getOp(id)
            case _ => ???
            }
        }
        case _ => NoOperand(id)
    }

    def getSymbol(id: String): Option[Symbol] = get(id) match {
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

class FuncTable(val id: String, val paramTypes: Seq[Type], var returnType: Type, val isPrivate: Boolean) extends Table {
    override def getReturnType = returnType
    def setReturnType(t: Type): Unit = {
        returnType = t
    }
}

object FuncTable {
    def apply(
        id: String, 
        paramTypes: Seq[Type], 
        returnType: Type, 
        isPrivate: Boolean = false
    ): FuncTable = new FuncTable(id, paramTypes, returnType, isPrivate)
}


sealed class ChildTable(val parent: Table) extends Table {
    override def getReturnType = parent.getReturnType
}
object ChildTable {
    def apply(parent: Table): ChildTable = new ChildTable(parent)
}

case class ClassTable(val class_id : String, val types: Seq[Type] = Seq()) extends Table {

    def getMethodTable(id: String): Option[MethodTable] = super.get(id) match {
        case Some(x) => x match {
            case x: MethodTable => Some(x)
            case x => {
                println(s"get $x")
                None
            } 
        }
        case None => None
    }
}

case class MethodTable(
    override val id: String, 
    override val paramTypes: Seq[Type], 
    t: Type, 
    override val isPrivate: Boolean = false,
    val parent: ClassTable
) extends FuncTable(id, paramTypes, t, isPrivate)

class Symbol(val t: Type, val isPrivate : Boolean = false) extends TableEntry 
object Symbol {
    def apply(t: Type): Symbol = new Symbol(t, false)
    def apply(t: Type, isPrivate: Boolean) = new Symbol(t, isPrivate)
}

case class ParamSymbol(override val t: Type) extends Symbol(t)
case class OpSymbol(override val t: Type, val op: Operand) extends Symbol(t)

class SymbolTable {

    private val table = MapM[String, FuncTable]()

    /* create global class table for storing all types of classes */
    val classes = MapM[String, ClassTable]()

    override def toString(): String = table.mkString("\n")

    def declare(id: String): FuncTable = declare(id, Seq(), AnyType)
    def declare(id: String, params: Seq[Param], returnType: Type): FuncTable = {
        val func = FuncTable(id, params.map(_.t), returnType)
        table(id) = func
        params.foreach(param => func.add(param.id, ParamSymbol(param.t)))
        return func
    }

    def get(id: String): Option[FuncTable] = table.get(id)

    def contains(id: String): Boolean = table.contains(id)

}