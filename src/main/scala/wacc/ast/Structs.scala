package wacc
package ast

import wacc.front.ParserBridge._
import parsley.genericbridges._
import wacc.back._

case class Member(t: Type, id: String)(val pos: (Int, Int))

object Member extends ParserBridgePos2[Type, String, Member]

case class Struct(struct_id: String, members: List[Member])(val pos: (Int, Int))

object Struct extends ParserBridgePos2[String, List[Member], Struct]

case class NewStruct(vals: List[RValue])(val pos: (Int, Int)) extends RValue 

object NewStruct extends ParserBridgePos1[List[RValue], NewStruct] 

