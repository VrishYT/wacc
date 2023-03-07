package wacc
package ast

sealed abstract class Flag

object Flag {
    def apply(id: String): Flag = id match {
        case "tailrec" => TailRecursiveFlag
        case _ => ??? 
    }
}

case object TailRecursiveFlag extends Flag


