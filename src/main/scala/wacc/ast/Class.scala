package wacc
package ast

import wacc.front.ParserBridge._

case class Field(isPrivate: Boolean, t: Type, id: String, rval: RValue)(val pos: (Int, Int))

object Field extends ParserBridgePos4[Boolean, Type, String, RValue, Field]

case class Class(class_id: String, decls: List[Field], funcs: List[Func])(val pos: (Int, Int))

object Class extends ParserBridgePos3[String, List[Field], List[Func], Class]

case class NewClass(class_id: String, vals: List[RValue])(val pos: (Int, Int)) extends RValue 

object NewClass extends ParserBridgePos2[String, List[RValue], NewClass] 




