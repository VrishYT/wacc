/* EXTENSION */

package wacc
package ast

import parsley.genericbridges.ParserBridge1

sealed abstract class Flag

object Flag extends ParserBridge1[String, Flag] {
    def apply(id: String): Flag = id match {
        case "tailrec" => TailRecursiveFlag
        case _ => ??? 
    }
}

case object TailRecursiveFlag extends Flag


