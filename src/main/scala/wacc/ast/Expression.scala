package wacc
package ast

import scala.collection.mutable.{ListBuffer}
import wacc.back._
import wacc.back.Condition._
import wacc.front.ParserBridge._

/* expressions extending right values */
trait Expr extends RValue

/* atomic types as case classes */
case class IntLiteral(x: Int)(val pos: (Int, Int)) extends Expr {
  override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Assembly = Assembly(ImmInt(x))
} 

case class CharLiteral(x: Char)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Assembly = Assembly(ImmInt(x))
}

case class StrLiteral(str: String)(val pos: (Int, Int)) extends Expr {
  override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable) : Assembly = {
    val label = symbolTable.addData(str)
    val reg = regs.allocate
    Assembly(reg, Seq(Load(reg, DataLabel(label))))
  }
}

case class BoolLiteral(x: Boolean)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Assembly = Assembly(ImmInt(if (x) 1 else 0), Seq())
}

/* operators as case classes */
case class UnaryOpExpr(op: UnaryOp, x: Expr)(val pos: (Int, Int)) extends Expr {

  override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Assembly = {

    val expr = x.toAssembly(regs, symbolTable)

    return op match {
      case Not => expr.not()
      case Ord | Chr => expr
      case Negate => {
        val out = regs.allocate
        val reg = regs.allocate
        Assembly(out, Seq(back.Mov(reg, ImmInt(0)), back.Sub(out, reg, expr.getOp)) ++ expr.instr)
      }
      case Length => ??? // TODO
    }
    
  }
}

case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends Expr {

  override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): Assembly = {

    def binaryOpToAssembly(out: Register, x: Register, y: Operand): Instruction = op match {
      case ast.Mul => Mul(out, x, y)
      case ast.Div => Div(out, x, y)
      case ast.Add => Add(out, x, y)
      case ast.Sub => Sub(out, x, y)
      case ast.And => And(out, x, y)
      case ast.Or => Or(out, x, y)
    }

    val expr1 = x.toAssembly(regs, symbolTable)
    val expr2 = y.toAssembly(regs, symbolTable)

    val instr = ListBuffer[Instruction]()
    instr ++= expr1.instr
    val reg = Operands.opToReg(expr1.getOp, regs)
    val r1 = reg._1
    instr ++= reg._2
    instr ++= expr2.instr

    val seq = expr1.instr ++ reg._2 ++ expr2.instr
    return op match {
      case ast.Greater => Assembly(seq :+ Cmp(r1, expr2.getOp), GT)
      case ast.GreaterEquals => Assembly(seq :+ Cmp(r1, expr2.getOp), GE)
      case ast.Less => Assembly(seq :+ Cmp(r1, expr2.getOp), LT)
      case ast.LessEquals => Assembly(seq :+ Cmp(r1, expr2.getOp), LE)
      case ast.Equal => Assembly(seq :+ Cmp(r1, expr2.getOp), EQ)
      case ast.NotEqual => Assembly(seq :+ Cmp(r1, expr2.getOp), NE)
      case _ => {
        val out = regs.allocate
        return Assembly(out, seq :+ binaryOpToAssembly(out, r1, expr2.getOp))
      }
    }
  }
}

/* expression parser bridges */
object IntLiteral extends ParserBridgePos1[Int, IntLiteral]

object CharLiteral extends ParserBridgePos1[Char, CharLiteral]

object StrLiteral extends ParserBridgePos1[String, StrLiteral]

object BoolLiteral extends ParserBridgePos1[Boolean, BoolLiteral]

/* case class for a pair literal, (always null) */
case class PairLiteralNull(val pos: (Int, Int)) extends Expr with ParserBridgePos0[Expr]

  