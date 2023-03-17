/* EXTENSION */

package wacc
package ast

import annotation.unused
import parsley.genericbridges.ParserBridge1
import scala.collection.mutable.{LinkedHashMap => MapM}

sealed abstract class Annotation(val errorMsg: String) {
    def isValid: Boolean = true
    def verify(func: Func): Boolean = false
    def process(func: Func)(implicit @unused funcTable: FuncTable): Func = func
}

object Annotation extends ParserBridge1[String, Annotation] {
    def apply(id: String): Annotation = id match {
        case "tailrec" => TailRecursiveAnnotation
        case _ => UnknownAnnotation
    }
}

case object UnknownAnnotation extends Annotation("Unknown annotation - shouldn't have ever reached this???") {
    override def isValid: Boolean = false
}

case object TailRecursiveAnnotation extends Annotation("Function is not tail-recursive\n - cannot optimize with \'@tailrec\' annotation") {

    override def verify(func: Func): Boolean = {
        import scala.annotation.nowarn
        var tailCallVar: Any = null
        @nowarn def verifyBranch(stats: List[Stat]): Boolean = {
            def matchId(ids: List[String], funcId: String): Boolean = ids.last == funcId.substring(funcId.indexOf("_") + 1)
            stats.foreach {
                case Declare(_, id, rhs) => rhs match {
                    case Call(ids, _) if (matchId(ids, func.fs._2)) => tailCallVar = Ident(id)(0,0) 
                    case _ => 
                }
                case AssignOrTypelessDeclare(lval, rval) => rval match {
                    case Call(ids, _) if (matchId(ids, func.fs._2)) => lval match {
                        case x: Ident => tailCallVar = x
                        case _ => 
                    }
                    case _ => 
                }
                case If(_, x, y) => {
                    val thenBranch = verifyBranch(x)
                    val tailVarSaved = tailCallVar
                    val elseBranch = verifyBranch(y)
                    if (tailCallVar == null) {
                        tailCallVar = tailVarSaved
                    } else if (tailVarSaved != null) {
                        ??? // TODO
                    }
                    if (thenBranch || elseBranch) return true
                }
                case Begin(xs) => verifyBranch(xs)
                case While(_, xs) => verifyBranch(xs)
                case Return(expr) => return tailCallVar == expr
                case Skip => 
                case _ => tailCallVar = null
            }
            false
        }

        verifyBranch(func.stats)
    }
    
    override def process(func: Func)(implicit funcTable: FuncTable): Func = {

        // import scala.collection.mutable.{Map => MapM}
        import scala.collection.mutable.ListBuffer

        def processStats(stats: List[Stat])(implicit table: Table): List[Stat] = {

            var ifCount = 0
            var whileCount = 0
            var beginCount = 0

            val out = ListBuffer[Stat]()

            def isModified(id: Ident): Boolean = table.getSymbol(id.id) match {
                case Some(x) => x.modified
                case _ => ???
            }

            def modifyVars(id: Ident, call: Call): Unit = {
                val callArgs = call.args
                val funcArgs = func.args.map(_.id).filter(_ != "this")
                table.getSymbol(id.id) match {
                    case Some(x) => x.modified = true
                    case _ => ???
                }
                val renamed = MapM[String, String]()
                funcArgs.zip(callArgs).foreach(x => {
                    val newId = s"_${x._1}"
                    renamed(x._1) = newId
                    val symbol = table.getSymbol(x._1) match {
                        case Some(x) => x
                        case None => ???
                    }
                    table.add(newId, Symbol(symbol.t, symbol.isPrivate))
                    out += Declare(symbol.t, newId, x._2)
                })
                renamed.foreach {
                    case (k, v) => out += AssignOrTypelessDeclare(Ident(k)(id.pos), Ident(v)(id.pos))
                } 
            }

            stats.foreach (stat => stat match {
                case Declare(_, id, rhs) => rhs match {
                    case call@Call(ids, _) if (ids.last == func.fs._2) => {
                        modifyVars(Ident(id)(0,0), call)
                    }
                    case _ => out += stat
                }
                case AssignOrTypelessDeclare(lval, rval) => lval match {
                    case x: LValue => rval match {
                        case call@Call(ids, _) if (ids.last == func.fs._2) => lval match {
                            case x: Ident => modifyVars(x, call)
                            case _ => ???
                        }
                        case _ => out += stat
                    }
                }
                case If(cond, x, y) => {
                    val thenChild = table.getTable(s"_if${ifCount}") match {
                        case Some(x) => x
                        case None => ???
                    }
                    val thenStats = processStats(x)(thenChild)
                    val elseChild = table.getTable(s"_else${ifCount}") match {
                        case Some(x) => x
                        case None => ???
                    }
                    val elseStats = processStats(y)(elseChild)
                    ifCount += 1
                    out += If(cond, thenStats, elseStats)
                }
                case While(cond, xs) => {
                    val child = table.getTable(s"_while${whileCount}") match {
                        case Some(x) => x
                        case None => ???
                    }
                    val newStats = processStats(xs)(child)
                    whileCount += 1
                    out += While(cond, newStats)
                }
                case Begin(xs) => {
                    val child = table.getTable(s"_begin${beginCount}") match {
                        case Some(x) => x
                        case None => ???
                    }
                    beginCount += 1
                    out += Begin(processStats(xs)(child))
                }
                case Return(expr) => expr match {
                    case x: Ident if isModified(x) => out += Continue(0,0)
                    case _ => out += stat
                }
                case _ => out += stat
            })
            out.toList
        }        
        
        // println(funcTable)
        val table = funcTable.table.clone()
        funcTable.table.filterInPlace((k, v) => v match {
            case _: OpSymbol => true
            case _ => false
        })
        val whileTable = ChildTable(funcTable)
        funcTable.addWhile(whileTable)
        // println(s"=${table.filter(_._2.isInstanceOf[ChildTable])}")

        def updateParents(table: MapM[String, TableEntry], parent: Table, function: Boolean = false): Unit = table.foreach { 
            case (id, entry) => entry match {
                case x: ChildTable => {
                    val clone = ChildTable(parent)
                    clone.id = id
                    clone.table ++= x.table
                    parent.table(id) = clone
                    updateParents(x.table, clone)
                }
                case _: OpSymbol if (function) => 
                case x => parent.table(id) = x  
            }
            case _ => ???
        }

        updateParents(table, whileTable, true)

        // table.foreach(entry => entry._2 match {
        //     case x: ChildTable => {
        //         val clone = ChildTable(whileTable)
        //         clone.id = entry._1
        //         println(s"${entry._1} has parent ${whileTable.id}")
        //         clone.table ++= x.table
        //         whileTable.table(entry._1) = clone
        //     }
        //     case _: OpSymbol => 
        //     case _ => whileTable.table(entry._1) = entry._2
        // })
        // println(s"=${funcTable.table.filter(_._2.isInstanceOf[ChildTable])}")
        // println(s"=${table.filter(_._2.isInstanceOf[ChildTable])}")
        // println("**")
        // println(whileTable.parent)
        // println("**")
        // println("++----------------------------")
        // println(whileTable)
        // println("++----------------------------")
        val stats = processStats(func.stats)(whileTable)
        
        // println(whileTable)
        // println(funcTable)

        // println(stats)

        TypedFunc(
            func.annotations, 
            func.isPrivate, 
            func.fs,
            func.args.toList,
            List(While(BoolLiteral(true)(0,0), stats :+ Return(IntLiteral(-1)(0,0))))
        )(func.pos)
    }

}


