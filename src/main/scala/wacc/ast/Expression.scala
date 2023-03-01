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
  override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {
    if (x < 0 || x > 256) {
      val out = gen.regs.allocate
      val instr: Seq[Instruction] = Seq(
        Load(out.getReg, DataLabel(x + ""))
      )
      RegAssembly(out.getReg, instr)
    } else Assembly(ImmInt(x))

  } 
} 

case class CharLiteral(x: Char)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = Assembly(ImmChar(x))
}

case class StrLiteral(str: String)(val pos: (Int, Int)) extends Expr {
  override def toAssembly(gen: CodeGenerator)(implicit table: Table) : Assembly = {
    val label = gen.text.add(str)
    Assembly(ImmLabel(label))
  }
}

case class BoolLiteral(x: Boolean)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = Assembly(ImmInt(if (x) 1 else 0), Seq(), if (x) AL else NO)
}

/* operators as case classes */
case class UnaryOpExpr(op: UnaryOp, x: Expr)(val pos: (Int, Int)) extends Expr {

  override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {

    val expr = x.toAssembly(gen)

    return op match {
      case Not => {
        expr.getOp match {
          case x: Register => Assembly(x, expr.instr ++ Seq(Xor(x, x, ImmInt(1))), expr.cond)
          case _ => expr.not()
        }
      }
      case Ord | Chr => expr
      case Negate => {
        val out = gen.regs.allocate
        val reg = gen.regs.allocate
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(IntegerOverflow)
        Assembly(out.getReg, (reg.instr :+ back.Mov(reg.getReg, ImmInt(0))) ++ out.instr ++ Seq (back.Sub(out.getReg, reg.getReg, expr.getOp), LinkBranch("_errOverflow", VS)) ++ expr.instr)
      }
      case Length => ??? // TODO
    }
    
  }
}

case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends Expr {

  override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {

    def binaryOpToAssembly(out: Register, x: Register, y: Operand): Seq[Instruction] = op match {
      case ast.Mul => {
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(IntegerOverflow)
        val out2 = gen.regs.allocate
        val reg = Operands.opToReg(y, gen.regs)
        reg.instr ++ Seq(
          SMull(out, out2.getReg, x, reg.getReg),
          Cmp(out2.getReg, ASR(out, ImmInt(31))),
          LinkBranch("_errOverflow", NE))}
      case ast.Div => {
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(DivZeroError)
        Seq(
          Mov(Register(0), x),
          Mov(Register(1), y),
          Cmp(Register(1), ImmInt(0)),
          LinkBranch("_errDivZero", EQ),
          DivMod(),
          Mov(out, Register(0))
        )
      }
      case ast.Add => {
        gen.postSections.addOne(IntegerOverflow)
        gen.postSections.addOne(PrintStringSection)
        Seq(
          Add(out, x, y),
          LinkBranch("_errOverflow", VS)
        )
      }
      case ast.Sub => {
        gen.postSections.addOne(IntegerOverflow)
        gen.postSections.addOne(PrintStringSection)
        Seq(
          Sub(out, x, y),
          LinkBranch("_errOverflow", VS)
        )
      }
      case ast.Mod => {
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(DivZeroError)
        Seq(
          Mov(Register(0), x),
          Mov(Register(1), y),
          Cmp(Register(1), ImmInt(0)),
          LinkBranch("_errDivZero", EQ),
          DivMod(),
          Mov(out, Register(1))
        )
      }
      case ast.And => Seq(And(out, x, y))
      case ast.Or => Seq(Or(out, x, y))
    }

    val expr1 = x.toAssembly(gen).condToReg(gen.regs)
    val expr2 = y.toAssembly(gen).condToReg(gen.regs)

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

  