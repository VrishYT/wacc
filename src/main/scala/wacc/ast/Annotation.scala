/* EXTENSION */

package wacc
package ast

import parsley.genericbridges.ParserBridge1

sealed abstract class Annotation(val errorMsg: String) {
    def isValid: Boolean = true
    def verify(func: Func): Boolean = false
}

object Annotation extends ParserBridge1[String, Annotation] {
    def apply(id: String): Annotation = id match {
        case "tailrec" => TailRecursiveAnnotation
        case _ => UnknownAnnotation
    }
}

case object TailRecursiveAnnotation extends Annotation("Function is not tail-recursive\n - cannot optimize with \'@tailrec\' annotation") {
    override def verify(func: Func): Boolean = {
        def verifyBranch(stats: List[Stat]): Boolean = {
            var lastVar = ""
            stats.foreach(stat => stat match {
                case If(_, x, y) => {
                    val thenBranch = verifyBranch(x)
                    val elseBranch = verifyBranch(y)

                    // TODO: OR/AND

                }
                case Begin(xs) => {
                    val branch = verifyBranch(xs)
                }
                case _ => 
            })
            true
        }

        verifyBranch(func.stats)
    }
}
case object UnknownAnnotation extends Annotation("Unknown annotation - shouldn't have ever reached this???") {
    override def isValid: Boolean = false
}


