/* EXTENSION */

package wacc
package ast

import parsley.genericbridges.ParserBridge1
import scala.collection.mutable.{LinkedHashMap => MapM}

sealed abstract class Annotation(val errorMsg: String) {
    def isValid: Boolean = true
    def verify(func: Func): Boolean = false
    def process(func: Func)(implicit funcTable: FuncTable): Func = func
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
                    case Call(ids, _) if (matchId(ids, func.fs._2)) => tailCallVar = lval
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

        // TODO

        val instr = ListBuffer[Stat]()

        def processStats(stats: List[Stat], cond: Option[(Expr, Boolean)] = None)(implicit table: MapM[String, TableEntry], child: Table): Unit = {
            var ifCount = 0
            var whileCount = 0
            var beginCount = 0

            val modifiedVars = ListBuffer[LValue]()
            val conditions = ListBuffer[(Expr, Boolean)]()

            def addConditional(out: Stat, cond: Expr, inverted: Boolean): Stat = {
                child.addIf(ChildTable(child), ChildTable(child))
                if (inverted) {
                    If(cond, List(Skip), List(out))
                } else {
                    If(cond, List(out), List(Skip))
                }

            }

            def modifyVars(lval: LValue, call: Call): Unit = {
                val callArgs = call.args
                val funcArgs = func.args.map(_.id).filter(_ != "this")
                modifiedVars += lval
                funcArgs.zip(callArgs).foreach(x => {
                    var out: Stat = AssignOrTypelessDeclare(Ident(x._1)(0,0), x._2)
                    out = cond match {
                        case Some(x) => x match {
                            case (x: Expr, inverted: Boolean) => addConditional(out, x, inverted)
                            case _ => ???
                        }
                        case _ => out
                    }
                    instr += out
                })

            }

            stats.foreach (stat => stat match {
                case Declare(_, id, rhs) => rhs match {
                    // TODO: if a call, re-assign params instead of passing in
                    case call@Call(ids, _) if (ids.last == func.fs._2) => {
                        cond match {
                            case Some(x) => {
                                conditions += x
                                println(conditions)
                            }
                            case _ => 
                        }
                        modifyVars(Ident(id)(0,0), call)
                    }
                    case _ => instr += stat
                }
                case AssignOrTypelessDeclare(lval, rval) => lval match {
                    case x: LValue => rval match {
                        case call@Call(ids, _) if (ids.last == func.fs._2) => {
                            cond match {
                                case Some(x) => {
                                    conditions += x
                                    println(conditions)
                                }
                                case _ => 
                            }
                            modifyVars(x, call)
                        }
                        case _ => instr += stat
                    }
                }
                case If(cond, x, y) => {
                    // TODO
                    val thenTable = table.get(s"_if${ifCount}") match {
                        case Some(x) => x match {
                            case x: Table => x.table
                            case _ => ???
                        }
                        case None => ???
                    } 
                    // TODO
                    processStats(x, Some(cond, false))(thenTable, child)
                    val elseTable = table.get(s"_else${ifCount}") match {
                        case Some(x) => x match {
                            case x: Table => x.table
                            case _ => ???
                        }
                        case None => ???
                    }
                    processStats(y, Some(cond, true))(elseTable, child)
                    // ifCount += 1
                }
                case While(_, xs) => {
                    // TODO
                    val childTable = table.get(s"_while${whileCount}") match {
                        case Some(x) => x match {
                            case x: Table => x.table
                            case _ => ???
                        }
                        case None => ???
                    }
                    processStats(xs)(childTable, child)
                    // whileCount += 1
                }
                case Begin(xs) => {
                    // TODO
                    val childTable = table.get(s"_begin${beginCount}") match {
                        case Some(x) => x match {
                            case x: Table => x.table
                            case _ => ???
                        }
                        case None => ???
                    }
                    processStats(xs)(childTable, child)
                    // beginCount += 1
                }
                case Return(expr) => expr match {
                    case x: LValue if modifiedVars.contains(x) =>
                    case _ => cond match {
                        case Some(x) => {
                            conditions += x
                            println(conditions)
                            instr += addConditional(stat, x._1, x._2)
                        }
                        // {
                        //     val out = if (x._2) If(x._1, List(Skip), List(stat)) else If(x._1, List(stat), List(Skip))
                        //     table.ifCount += 1
                        //     instr += out
                        // }
                        case None => instr += stat
                    }
                }
                case _ => cond match {
                    case Some(x) => instr += addConditional(stat, x._1, x._2)
                    case None => instr += stat
                }
            })

            // println(s"modified = $modifiedVars")
        }

        val table = funcTable.table.clone()
        // println(funcTable)
        funcTable.table.filterInPlace((k, v) => v match {
            case _: Symbol => true
            case _ => false
        })
        val whileTable = ChildTable(funcTable)
        processStats(func.stats)(table, whileTable)
        funcTable.addWhile(whileTable)

        // println(instr)

        TypedFunc(
            func.annotations, 
            func.isPrivate, 
            func.fs,
            func.args.toList,
            List(While(BoolLiteral(true)(0,0), instr.toList))
        )(func.pos)
    }

}


