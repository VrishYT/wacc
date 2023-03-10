/* EXTENSION */

package wacc
package ast

import parsley.genericbridges.ParserBridge1

sealed abstract class Annotation(val errorMsg: String) {
    def isValid: Boolean = true
    def verify(func: Func): Boolean = false
    def process(stats: List[Stat]): List[Stat] = stats
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
            stats.foreach(stat => stat match {
                case Declare(_, id, rhs) => rhs match {
                    case Call(funcId :: Nil, _) if (funcId == func.fs._2) => tailCallVar = Ident(id)(0,0) 
                    case _ => 
                }
                case AssignOrTypelessDeclare(lval, rval) => lval match {
                    case x: LExpr => rval match {
                        case Call(funcId, _) if (funcId == func.fs._2) => tailCallVar = x
                        case _ => 
                    }
                }
                case If(_, x, y) => {

                    def endsInReturn(stats: List[Stat]): Boolean = stats.last match {
                        case x: Return => true
                        case _ => false
                    }

                    val thenBranch = verifyBranch(x)
                    val elseBranch = verifyBranch(y)
                    if (thenBranch) return endsInReturn(y)
                    else if (elseBranch) return endsInReturn(x)
                }
                case Begin(xs) => {
                    verifyBranch(xs)
                }
                case Return(expr) => {
                    return tailCallVar == expr
                }
                case _ => 
            })
            false
        }

        verifyBranch(func.stats)
    }
    
    override def process(stats: List[Stat]): List[Stat] = {

        import scala.collection.mutable.ListBuffer

        val beforeRecursiveIf = ListBuffer[Stat]()
        // TODO
        stats
    }

}


