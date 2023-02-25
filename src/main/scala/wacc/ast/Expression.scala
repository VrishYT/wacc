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
  override def toAssembly(gen: CodeGenerator): Assembly = Assembly(ImmInt(x))
} 

case class CharLiteral(x: Char)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(gen: CodeGenerator): Assembly = Assembly(ImmChar(x))
}

case class StrLiteral(str: String)(val pos: (Int, Int)) extends Expr {
  override def toAssembly(gen: CodeGenerator) : Assembly = {
    val label = gen.text.add(str)
    Assembly(ImmLabel(label))
  }
}

case class BoolLiteral(x: Boolean)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(gen: CodeGenerator): Assembly = Assembly(ImmInt(if (x) 1 else 0), Seq())
}

/* operators as case classes */
case class UnaryOpExpr(op: UnaryOp, x: Expr)(val pos: (Int, Int)) extends Expr {

  override def toAssembly(gen: CodeGenerator): Assembly = {

    val expr = x.toAssembly(gen)

    return op match {
      case Not => expr.not()
      case Ord | Chr => expr
      case Negate => {
        val out = gen.regs.allocate
        val reg = gen.regs.allocate
        Assembly(out.getReg, (reg.instr :+ back.Mov(reg.getReg, ImmInt(0))) ++ out.instr ++ (back.Sub(out.getReg, reg.getReg, expr.getOp) +: expr.instr))
      }
      case Length => ??? // TODO
    }
    
  }
}

case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends Expr {

  override def toAssembly(gen: CodeGenerator): Assembly = {

    def binaryOpToAssembly(out: Register, x: Register, y: Operand): Seq[Instruction] = op match {
      case ast.Mul => {
        val out2 = gen.regs.allocate
        Seq(
          SMull(out2.getReg, out, x, y),
          Cmp(out, ASR(out2.getReg, ImmInt(31))),
          LinkBranch("_errOverflow", NE))}
      case ast.Div => {
        Seq(
          //TODO add error Div Zero to datasection 
          // TODO add prints to datasection 
          Mov(Register(0), x),
          Mov(Register(1), y),
          Cmp(Register(1), ImmInt(0)),
          LinkBranch("_errDivZero", EQ),
          Div(),
          Mov(out, Register(0))
        )
      }
      case ast.Add => Seq(
        Add(out, x, y),
        LinkBranch("_errOverflow", VS))
      case ast.Sub => Seq(
        //TODO add error Integer Overflow to datasection 
        // TODO add prints to datasection 
        Sub(out, x, y),
        LinkBranch("_errOverflow", VS))
      case ast.And => Seq(And(out, x, y))
      case ast.Or => Seq(Or(out, x, y))
    }

    val expr1 = x.toAssembly(gen)
    val expr2 = y.toAssembly(gen)

    val instr = ListBuffer[Instruction]()
    instr ++= expr1.instr
    val reg = Operands.opToReg(expr1.getOp, gen.regs)
    val r1 = reg.getReg
    instr ++= reg.instr
    instr ++= expr2.instr

    val seq = expr1.instr ++ reg.instr ++ expr2.instr
    return op match {
      case ast.Greater => Assembly(seq :+ Cmp(r1, expr2.getOp), GT)
      case ast.GreaterEquals => Assembly(seq :+ Cmp(r1, expr2.getOp), GE)
      case ast.Less => Assembly(seq :+ Cmp(r1, expr2.getOp), LT)
      case ast.LessEquals => Assembly(seq :+ Cmp(r1, expr2.getOp), LE)
      case ast.Equal => Assembly(seq :+ Cmp(r1, expr2.getOp), EQ)
      case ast.NotEqual => Assembly(seq :+ Cmp(r1, expr2.getOp), NE)
      case _ => {
        val out = gen.regs.allocate
        return Assembly(out.getReg, (seq ++ out.instr) ++ binaryOpToAssembly(out.getReg, r1, expr2.getOp))
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

  