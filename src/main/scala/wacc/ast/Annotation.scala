/* EXTENSION */

package wacc
package ast

import parsley.genericbridges.ParserBridge1

sealed abstract class Annotation

object Annotation extends ParserBridge1[String, Annotation] {
    def apply(id: String): Annotation = id match {
        case "tailrec" => TailRecursiveAnnotation
        case _ => ???
    }
}

case object TailRecursiveAnnotation extends Annotation


