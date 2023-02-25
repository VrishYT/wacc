package wacc.ast

import wacc.front.ParserBridge._
import wacc.back._

/* left expressions extending expressions and left values */
sealed trait LExpr extends Expr with LValue {
  def pos: (Int, Int)
  override def toAssembly(gen: CodeGenerator): RegAssembly = TODOAssembly
}

/* expressions extending left expressions */
case class Ident(id: String)(val pos: (Int, Int)) extends LExpr {
  override def toAssembly(gen: CodeGenerator): RegAssembly = RegAssembly(gen.regs.get(id))
}

object Ident extends ParserBridgePos1[String, Ident]

case class ArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr
  
object ArrayElem extends ParserBridgePos2[String, List[Expr], ArrayElem]
  
case class IdentOrArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr

/* identifier or array elements with parser bridge */
object IdentOrArrayElem extends ParserBridgePos2[String, Option[List[Expr]], LExpr] {
    def apply(id: String, xs: Option[List[Expr]])(pos: (Int, Int)): LExpr = xs match {
      case None => Ident(id)(pos)
      case Some(xs) => ArrayElem(id, xs)(pos)
    }
}



