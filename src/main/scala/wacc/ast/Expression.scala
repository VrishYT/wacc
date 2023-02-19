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
  override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): (Operand, Seq[Instruction]) = (ImmInt(x), Seq())
} 

case class CharLiteral(x: Char)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): (Operand, Seq[Instruction]) = (ImmChar(x), Seq())
}

case class StrLiteral(str: String)(val pos: (Int, Int)) extends Expr {
  override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable) : (Operand, Seq[Instruction]) = {
    val label = symbolTable.addData(str)
    val reg = regs.allocate
    (reg, Seq(Load(reg, DataLabel(label))))
  }
}

case class BoolLiteral(x: Boolean)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): (Operand, Seq[Instruction]) = (ImmInt(if (x) 1 else 0), Seq())
}

/* operators as case classes */
case class UnaryOpExpr(op: UnaryOp, x: Expr)(val pos: (Int, Int)) extends Expr {

  override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): (Operand, Seq[Instruction]) = {

    def unaryOpToAssembly(out: Register, x: Register): Seq[Instruction] = op match {
      case Not => Seq(back.Xor(out, x, ImmInt(1)))
      case Negate => {
        val reg = regs.allocate
        Seq(back.Mov(reg, ImmInt(0)), back.Sub(out, reg, x))
      }
      case Length => Seq()
    }

    val expr = x.toAssembly(regs, symbolTable)
    val reg = Operands.opToReg(expr._1, regs)
    val out = regs.allocate

    return op match {
      case Ord => (expr._1, Seq())
      case Chr => (expr._1, Seq())
      case _ => (out, expr._2 ++ reg._2 ++ unaryOpToAssembly(out, reg._1))
    }
    
  }
}

case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends Expr {

  override def toAssembly(regs: RegisterAllocator, symbolTable: SymbolTable): (Operand, Seq[Instruction]) = {

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
    instr ++= expr1._2
    val reg = Operands.opToReg(expr1._1, regs)
    val r1 = reg._1
    instr ++= reg._2
    instr ++= expr2._2

    return op match {
      case ast.Greater => (ImmInt(-1), Seq(Cmp(r1, expr2._1, GT)))
      case ast.GreaterEquals => (ImmInt(-1), Seq(Cmp(r1, expr2._1, GE))) 
      case ast.Less => (ImmInt(-1), Seq(Cmp(r1, expr2._1, LT)))
      case ast.LessEquals => (ImmInt(-1), Seq(Cmp(r1, expr2._1, LE)))
      case ast.Equal => (ImmInt(-1), Seq(Cmp(r1, expr2._1, EQ)))
      case ast.NotEqual => (ImmInt(-1), Seq(Cmp(r1, expr2._1, NE)))
      case _ => {
        val out = regs.allocate
        instr += binaryOpToAssembly(out, r1, expr2._1)
        return (out, instr.toSeq)
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

  