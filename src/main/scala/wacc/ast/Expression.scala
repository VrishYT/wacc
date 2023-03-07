package wacc
package ast

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
        Load(out.getReg(), DataLabel(x.toString()))
      )
      RegAssembly(out.getReg(), instr)
    } else Assembly(ImmInt(x))

  } 
} 

case class CharLiteral(x: Char)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = Assembly(ImmChar(x))
}

case class StrLiteral(str: String)(val pos: (Int, Int)) extends Expr {
  override def toAssembly(gen: CodeGenerator)(implicit table: Table) : Assembly = {
    val label = gen.text.add(str)
    Assembly(DataLabel(label))
  }
}

case class BoolLiteral(x: Boolean)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = Assembly(ImmInt(if (x) 1 else 0), Seq(), if (x) AL else NO)
}

/* operators as case classes */
case class UnaryOpExpr(op: UnaryOp, x: Expr)(val pos: (Int, Int)) extends Expr {

  override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {

    def len(expr: Assembly): Assembly = {
      val out = Operands.opToReg(expr.getOp(), gen.regs)
      return Assembly(out.getReg(), expr.instr ++ out.instr ++ Seq(Load(out.getReg(), Address(out.getReg(), ImmInt(-4)))))
    }

    val expr = x.toAssembly(gen)

    return op match {
      case Not => {
        expr.getOp() match {
          case x: Register => Assembly(x, expr.instr ++ Seq(Xor(x, x, ImmInt(1))), expr.cond)
          case _ => expr.not()
        }
      }
      case Ord | Chr => x match {
        case _: Ident => {
          val reg = gen.regs.allocate
          // println(s"allocated ${reg.getReg()} for ident")
          gen.regs.free(reg.getReg())
          RegAssembly(reg.getReg(), expr.instr ++ reg.instr :+ Mov(reg.getReg(), expr.getOp()))
        }
        case _ => expr
      }
      case Negate => {
        val out = gen.regs.allocate
        val reg = gen.regs.allocate
        gen.regs.free(reg.getReg())
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(IntegerOverflow)
        Assembly(out.getReg(), (reg.instr :+ back.Mov(reg.getReg(), ImmInt(0))) ++ out.instr ++ Seq (back.Sub(out.getReg(), reg.getReg(), expr.getOp()), LinkBranch("_errOverflow", VS)) ++ expr.instr)
      }
      case Length => {
        x match {
          case Ident(id) => {
            table.getType(id) match {
              case Some(x) => x match {
                case _: ArrayType => len(expr)
                case _ => ???
              }
              case None => ???
            }
          }
          case _: ArrayElem => len(expr)
          case _ => ???
        }
      }
    }
    
  }
}

case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends Expr {

  override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {

    // println(s"*--x $op y--*\n")
    // println(x + "\n")
    // println(y + "\n")

    def binaryOpToAssembly(out: Register, x: Register, y: Operand): Seq[Instruction] = op match {
      case ast.Mul => {
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(IntegerOverflow)
        val temp = Register(12)
        // println("optoreg")
        val op = Operands.opToReg(y, gen.regs)
        y match {
          case _: LExpr => 
          case _ => gen.regs.free(op.getReg())
        }
        // println("after optoreg")
        op.instr ++ Seq(
          SMull(out, temp, x, op.getReg()),
          Cmp(temp, ASR(out, ImmInt(31))),
          LinkBranch("_errOverflow", NE)
        )
      }
      case ast.Div => {
        gen.postSections.addOne(PrintStringSection)
        gen.postSections.addOne(DivZeroError)
        Seq(
          Push(Register(1), Register(2), Register(3)),
          Mov(Register(0), x),
          Mov(Register(1), y),
          Cmp(Register(1), ImmInt(0)),
          LinkBranch("_errDivZero", EQ),
          DivMod,
          Pop(Register(1), Register(2), Register(3)),
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
          Push(Register(1), Register(2), Register(3)),
          Mov(Register(0), x),
          Mov(Register(1), y),
          Cmp(Register(1), ImmInt(0)),
          LinkBranch("_errDivZero", EQ),
          DivMod,
          Mov(Register(0), Register(1)),
          Pop(Register(1), Register(2), Register(3)),
          Mov(out, Register(0))
        )
      }
      case ast.And => Seq(And(out, x, y))
      case ast.Or => Seq(Or(out, x, y))
      case _ => ???
    }

    val expr1 = x.toAssembly(gen).condToReg(gen.regs)
    val expr2 = y.toAssembly(gen).condToReg(gen.regs)

    val reg = Operands.opToReg(expr1.getOp(), gen.regs)
    val r1 = reg.getReg()

    def evalBool(cond: Condition): Assembly = Assembly(
      (expr1.instr ++ reg.instr ++ expr2.instr) :+ Cmp(r1, expr2.getOp()), 
      cond
    )

    return op match {
      case ast.Greater => evalBool(GT)
      case ast.GreaterEquals => evalBool(GE)
      case ast.Less => evalBool(LT)
      case ast.LessEquals => evalBool(LE)
      case ast.Equal => evalBool(EQ)
      case ast.NotEqual => evalBool(NE)
      case _ => {
        val out = x match {
          case _: Ident => {
            val reg = gen.regs.allocate
            // println(s"allocated ${reg.getReg()} for ident")
            reg
          }
          case x => {
            // println(x)
            RegAssembly(r1)
          }
        }
        val op2 = expr2.getOp() match {
          case reg: Register => {
            y match {
              case x: LExpr => 
              case _ => gen.regs.free(reg)
            }
            reg
          }
          case x => x
        }
        // println(s"regOut = ${out.getReg()}")
        val assembly = Assembly(out.getReg(), expr2.instr ++ expr1.instr ++ reg.instr ++ out.instr ++ binaryOpToAssembly(out.getReg(), r1, op2))
        gen.regs.free(out.getReg())
        // println("---")
        assembly
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
case class PairLiteralNull(val pos: (Int, Int)) extends Expr with ParserBridgePos0[Expr] {
  override def toAssembly(gen: CodeGenerator)(implicit table: Table): Assembly = {
    val regAss = gen.regs.allocate
    val reg = regAss.getReg()
    val regInstr = regAss.instr
    val instrns =  Seq(Mov(reg, ImmInt(0)))
    return Assembly(reg, regInstr ++ instrns.toSeq)
  }
}

  