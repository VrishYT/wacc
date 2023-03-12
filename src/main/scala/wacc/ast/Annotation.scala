/* EXTENSION */

package wacc
package ast

import parsley.genericbridges.ParserBridge1

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

        // // baseCases = List((Condition, isInverted), ReturnExpr)
        // val baseCases = ListBuffer[(Option[(Expr, Boolean)], Expr)]() 
        // // recursiveCalls = List((Condition, isInverted), ReturnExpr)
        // val recursiveCalls = ListBuffer[(Option[(Expr, Boolean)], Expr)]()

        // /* recursive function to look for base case(s) and recursive call(s) */
        // def findReturns(stats: List[Stat], cond: Option[(Expr, Boolean)]): Unit = {
        //     var tailCallVar: Any = null
        //     stats.foreach {
        //         case Declare(_, id, rhs) => rhs match {
        //             case Call(ids, _) if (ids.last == func.fs._2) => tailCallVar = Ident(id)(0,0) 
        //             case _ => 
        //         }
        //         case AssignOrTypelessDeclare(lval, rval) => lval match {
        //             case x: LExpr => rval match {
        //                 case Call(ids, _) if (ids.last == func.fs._2) => tailCallVar = x
        //             case _ => 
        //             }
        //         }
        //         case If(cond, x, y) => {
        //             findReturns(x, Some(cond, false))
        //             findReturns(y, Some(cond, true))
        //         }
        //         case Begin(xs) => findReturns(xs, None)
        //         case Return(expr) => if (tailCallVar == expr) {
        //             recursiveCalls += ((cond, expr))
        //         } else {
        //             baseCases += ((cond, expr))
        //         }
        //         case _ => 
        //     }
        // }

        // findReturns(func.stats, None)
        // println(s"base: $baseCases")
        // println(s"rec : $recursiveCalls")

        val instr = ListBuffer[Stat]()

        def processStats(stats: List[Stat], cond: Option[(Expr, Boolean)] = None)(implicit table: Table): Unit = {
            var ifCount = 0
            var whileCount = 0
            var beginCount = 0

            val modifiedVars = ListBuffer[LValue]()

            def modifyVars(lval: LValue, call: Call): Unit = {
                val callArgs = call.args
                val funcArgs = func.args.map(_.id).filter(_ != "this")
                modifiedVars += lval
                funcArgs.zip(callArgs).foreach(x => {
                    var out: Stat = AssignOrTypelessDeclare(Ident(x._1)(0,0), x._2)
                    out = cond match {
                        case Some(x) => x match {
                            case (x: Expr, inverted: Boolean) => {
                                if (inverted) {
                                    If(x, List(Skip), List(out))
                                } else {
                                    If(x, List(out), List(Skip))
                                }
                            }
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
                    case call@Call(ids, _) if (ids.last == func.fs._2) => modifyVars(Ident(id)(0,0), call)
                    case _ => instr += stat
                }
                case AssignOrTypelessDeclare(lval, rval) => lval match {
                    case x: LValue => rval match {
                        case call@Call(ids, _) if (ids.last == func.fs._2) => modifyVars(x, call)
                        case _ => instr += stat
                    }
                }
                case If(cond, x, y) => {
                    // TODO
                    val thenTable = table.getTable(s"_if${ifCount}") match {
                        case Some(x) => x 
                        case None => ???
                    } 
                    // TODO
                    processStats(x, Some(cond, false))(thenTable)
                    val elseTable = table.getTable(s"_else${ifCount}") match {
                        case Some(x) => x 
                        case None => ???
                    }
                    processStats(y, Some(cond, true))(elseTable)
                    ifCount += 1
                }
                case While(_, xs) => {
                    // TODO
                    val child = table.getTable(s"_while${whileCount}") match {
                        case Some(x) => x 
                        case None => ???
                    }
                    processStats(xs)(child)
                    whileCount += 1
                }
                case Begin(xs) => {
                    // TODO
                    val child = table.getTable(s"_begin${beginCount}") match {
                        case Some(x) => x 
                        case None => ???
                    }
                    processStats(xs)(child)
                    beginCount += 1
                }
                case Return(expr) => expr match {
                    case x: LValue if modifiedVars.contains(x) => 
                    case _ => instr += stat
                }
                case _ => 
            })

            // println(s"modified = $modifiedVars")
        }

        processStats(func.stats)

        // println(instr)

        TypedFunc(
            func.annotations, 
            func.isPrivate, 
            func.fs,
            func.args,
            instr.toList
        )(func.pos)
    }

}


