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
        @nowarn def verifyBranch(stats: List[Stat]): Boolean = {
            var tailCallVar: Any = null
            def matchId(ids: List[String], funcId: String): Boolean = ids.last == funcId.substring(funcId.indexOf("_") + 1)
            stats.foreach(stat => stat match {
                case Declare(_, id, rhs) => rhs match {
                    case Call(ids, _) if (matchId(ids, func.fs._2)) => tailCallVar = Ident(id)(0,0) 
                    case _ => 
                }
                case AssignOrTypelessDeclare(lval, rval) => lval match {
                    case x: LExpr => rval match {
                        case Call(ids, _) if (matchId(ids, func.fs._2)) => tailCallVar = x
                        case _ => 
                    }
                }
                case If(_, x, y) if (verifyBranch(x) || verifyBranch(y)) => return true
                case Begin(xs) => verifyBranch(xs)
                case While(_, xs) => verifyBranch(xs)
                case Return(expr) =>  return tailCallVar == expr
                case _ => 
            })
            false
        }

        verifyBranch(func.stats)
    }
    
    override def process(func: Func)(implicit funcTable: FuncTable): Func = {

        import scala.collection.mutable.{Map => MapM}
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

        def processStats(stats: List[Stat])(implicit table: Table): Unit = {
            val modifiedVars = MapM[LValue, LValue]()
            def modifyVars(lval: LValue, call: Call): Unit = {

            }

            stats.foreach {
                case Declare(_, id, rhs) => rhs match {
                    // TODO: if a call, re-assign params instead of passing in
                    case call@Call(ids, _) if (ids.last == func.fs._2) => modifyVars(LExpr(ids, None)(0,0), call)
                    case _ => 
                }
                case AssignOrTypelessDeclare(lval, rval) => lval match {
                    case x: LValue => rval match {
                        case call@Call(ids, _) if (ids.last == func.fs._2) => modifyVars(x, call)
                        case _ => 
                    }
                }
                case If(_, x, y) => {
                    // TODO
                    val thenTable = table.getTable("if") match {
                        case Some(x) => x 
                        case None => ???
                    } 
                    // TODO
                    processStats(x)(thenTable)
                    val elseTable = table.getTable("else") match {
                        case Some(x) => x 
                        case None => ???
                    }
                    processStats(y)(elseTable)
                }
                case While(_, xs) => {
                    // TODO
                    val child = table.getTable("while") match {
                        case Some(x) => x 
                        case None => ???
                    }
                    processStats(xs)(child)
                }
                case Begin(xs) => {
                    // TODO
                    val child = table.getTable("begin") match {
                        case Some(x) => x 
                        case None => ???
                    }
                    processStats(xs)(child)
                }
                case Return(expr) => expr match {
                    case x: LValue => 
                    case _ => 
                }
                case _ => 
            }
        }

        processStats(func.stats)

        TypedFunc(
            func.annotations, 
            func.isPrivate, 
            func.fs,
            func.args,
            instr.toList
        )(func.pos)
    }

}


