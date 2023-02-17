package wacc

object AST {

  import parsley.genericbridges._
  import front.ParserBridge._
  import back._
  import scala.collection.LinearSeq
  import scala.collection.mutable.ListBuffer
  import Condition._

  /* program case class with its functions and statements */
  case class Program(fs: List[Func], stats: List[Stat]) {
    def toAssembly(regs: RegisterAllocator): Seq[Instruction] = {

      // val assFunc = fs.map(_.toAssembly(regs))
      // val assStat = stats.map(_.toAssembly(regs))
      
      // TODO: concat all
      // assStat in 'main'

      return Seq()
    }
  }

  /* program companion object with parser bridge */
  object Program extends ParserBridge2[List[Func], List[Stat], Program]

  /* function case class with position */
  case class Func(fs: (Type, String), args: List[Param], stats: List[Stat])(val pos: (Int, Int)) {

    def toAssembly(regs: RegisterAllocator): Seq[Instruction] = {
      // TODO: function assembly
      val assStat = stats.map(_.toAssembly(regs))
      return Seq()
    }

    /* define validReturn of a function, and match on the last statement : */
    def validReturn: Boolean = validReturn(stats)

    def validReturn(stats: List[Stat]): Boolean = stats.last match {

      /* return true if it's a return or exit */
      case _: Return | _: Exit => true

      /* check the other valid case for begin or if statements */
      case _ => {
        var valid = false

        /* check if each branch of any new scope has a valid return, valid
                   will remain false if they don't */
        stats.foreach(stat => stat match {
          case If(_, x, y) => {
            valid |= validReturn(x) && validReturn(y)
          }
          case Begin(xs) => {
            valid |= validReturn(xs)
          }
          case _ =>
        })

        /* return valid once all eligible paths of control flow
                   have been checked */
        valid
      }
    }
  }

  /* parameter case class with position */
  case class Param(t: Type, id: String)(val pos: (Int, Int)) {
    def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq() // TODO: param to assembly
  }

  /* function and parameter companion objects with parser bridges */
  object Func extends ParserBridgePos3[(Type, String), List[Param], List[Stat], Func]

  object Param extends ParserBridgePos2[Type, String, Param]

  /* statements as objects extending the sealed trait Stat */
  sealed trait Stat {
    def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq()
  }

  case object Skip extends Stat with ParserBridge0[Stat]

  case class Declare(t: Type, id: String, rhs: RValue) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq()
  }

  case class Assign(x: LValue, y: RValue) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq() 
  }

  case class Read(x: LValue) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq() 
  }

  case class Free(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq() 
  }

  case class Return(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq() 
  }

  case class Exit(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = {
      val ass = x.toAssembly(regs)
      ass._2 ++ Seq(
                Mov(Register(0), ass._1),
                LinkBranch("exit"))
    }
  }

  case class Print(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq() 
  }

  case class Println(x: Expr) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq() 
  }

  case class If(p: Expr, x: List[Stat], y: List[Stat]) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq() 
  }

  case class While(p: Expr, x: List[Stat]) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq() 
  }

  case class Begin(xs: List[Stat]) extends Stat {
    override def toAssembly(regs: RegisterAllocator): Seq[Instruction] = Seq()
  }

  /* parser bridges for statements */
  object Declare extends ParserBridge3[Type, String, RValue, Declare]

  object Assign extends ParserBridge2[LValue, RValue, Assign]

  object Read extends ParserBridge1[LValue, Read]

  object Free extends ParserBridge1[Expr, Free]

  object Return extends ParserBridge1[Expr, Return]

  object Exit extends ParserBridge1[Expr, Exit]

  object Print extends ParserBridge1[Expr, Print]

  object Println extends ParserBridge1[Expr, Println]

  object If extends ParserBridge3[Expr, List[Stat], List[Stat], If]

  object While extends ParserBridge2[Expr, List[Stat], While]

  object Begin extends ParserBridge1[List[Stat], Begin]

  /* left values as a sealed trait with a position */
  sealed trait LValue {
    def pos: (Int, Int)
  }

  /* pair elements as a sealed trait with a position */
  sealed trait PairElem extends LValue with RValue

  case class Fst(x: LValue)(val pos: (Int, Int)) extends PairElem

  case class Snd(x: LValue)(val pos: (Int, Int)) extends PairElem

  object Fst extends ParserBridgePos1[LValue, Fst]

  object Snd extends ParserBridgePos1[LValue, Snd]

  /* right values as a sealed trait with a position attribute */
  sealed trait RValue {
    def pos: (Int, Int)
  }

  /* case classes for right values */
  case class ArrayLiteral(xs: List[Expr])(val pos: (Int, Int)) extends RValue

  case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends RValue

  case class Call(id: String, args: List[Expr])(val pos: (Int, Int)) extends RValue

  /* companion objects for right values */
  object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral]

  object NewPair extends ParserBridge2[Expr, Expr, NewPair] {
    def apply(fst: Expr, snd: Expr) = new NewPair(fst, snd)(fst.pos, snd.pos)
  }

  object Call extends ParserBridgePos2[String, List[Expr], Call]

  /* types */
  sealed trait Type

  /* case class for array type */
  case class ArrayType(t: Type) extends Type {

    /* override equals to also return true if either type is of AnyType */
    override def equals(that: Any): Boolean = {
      that match {
        case ArrayType(t) => {
          if (this.t == AnyType || t == AnyType) {
            return true
          } else {
            return this.t == t
          }
        }
        case _ => return false
      }
    }
  }

  /* case class for pair types */
  case class PairType(fst: Type, snd: Type) extends Type {

    /* override equals to return true if being compared to a type deleted pair */
    override def equals(that: Any): Boolean = {
      def pairEq(p: PairType) = {
        this.fst == p.fst && this.snd == p.snd
      }

      that match {
        case x: PairType => pairEq(x)
        case x: BaseType => false
        case Pair => true
        case _ => false
      }
    }
  }

  /* companion objects for array and pair types, with parser bridges */
  object ArrayType extends ParserBridge1[Type, ArrayType]

  object PairType extends ParserBridge2[Type, Type, PairType]

  /* base types extending type */
  sealed trait BaseType extends Type

  case object IntType extends BaseType with ParserBridge0[BaseType] {
    def apply() = IntType
  }

  case object BoolType extends BaseType with ParserBridge0[BaseType] {
    def apply() = BoolType
  }

  case object CharType extends BaseType with ParserBridge0[BaseType] {
    def apply() = CharType
  }

  case object StringType extends BaseType with ParserBridge0[BaseType] {
    def apply() = StringType
  }

  case object AnyType extends BaseType {
    override def equals(that: Any): Boolean = true
  }

  /* the case object for a type deleted pair type */
  case object Pair extends Type with ParserBridge0[Type] {

    /* override equals so that a type deleted pair type can match any other pair type */
    override def equals(that: Any): Boolean = that match {
      case _: BaseType => false
      case _: ArrayType => false
      case _ => true
    }
  }

  /* expressions extending right values */
  sealed trait Expr extends RValue {
    def toAssembly(regs: RegisterAllocator): (Operand, Seq[Instruction]) = (ImmInt(0), Seq())
  }

  /* atomic types as case classes */
  case class IntLiteral(x: Int)(val pos: (Int, Int)) extends Expr {
    override def toAssembly(regs: RegisterAllocator): (Operand, Seq[Instruction]) = (ImmInt(x), Seq())
  } 

  case class CharLiteral(x: Char)(val pos: (Int, Int)) extends Expr {
     override def toAssembly(regs: RegisterAllocator): (Operand, Seq[Instruction]) = (ImmChar(x), Seq())
  }

  case class StrLiteral(xs: String)(val pos: (Int, Int)) extends Expr

  case class BoolLiteral(x: Boolean)(val pos: (Int, Int)) extends Expr {
     override def toAssembly(regs: RegisterAllocator): (Operand, Seq[Instruction]) = (ImmInt(if (x) 1 else 0), Seq())
  }

  /* operators as case classes */
  case class UnaryOpExpr(op: UnaryOp, x: Expr)(val pos: (Int, Int)) extends Expr {

    override def toAssembly(regs: RegisterAllocator): (Operand, Seq[Instruction]) = {

      def unaryOpToAssembly(out: Register, x: Register): Seq[Instruction] = op match {
        case Not => Seq(back.Xor(out, x, ImmInt(1)))
        case Negate => {
          val reg = regs.allocate
          Seq(back.Mov(reg, ImmInt(0)), back.Sub(out, reg, x))
        }
        case Length => Seq()
      }

      val expr = x.toAssembly(regs)
      val reg = Operands.opToReg(expr._1, regs)
      val out = regs.allocate

      return op match {
        case Ord => (expr._1, Seq())
        case Chr => (expr._1, Seq())
        case _ => (out, expr._2 ++ reg._2 ++ unaryOpToAssembly(out, reg._1))
      }
      
      //return (out, expr._2 ++ reg._2 ++ unaryOpToAssembly(out, reg._1))
    }
  }

  case class BinaryOpExpr(op: BinaryOp, x: Expr, y: Expr)(val pos: (Int, Int), val pos2: (Int, Int)) extends Expr {

    override def toAssembly(regs: RegisterAllocator): (Operand, Seq[Instruction]) = {

      def binaryOpToAssembly(out: Register, x: Register, y: Operand): Instruction = op match {
        case Mul => back.Mul(out, x, y)
        case Div => back.Div(out, x, y)
        case Add => back.Add(out, x, y)
        case Sub => back.Sub(out, x, y)
        case And => back.And(out, x, y)
        case Or => back.Or(out, x, y)
      }

      val expr1 = x.toAssembly(regs)
      val expr2 = y.toAssembly(regs)

      val instr = ListBuffer[Instruction]()
      instr ++= expr1._2
      val reg = Operands.opToReg(expr1._1, regs)
      val r1 = reg._1
      instr ++= reg._2
      instr ++= expr2._2

      return op match {
        case Greater => (ImmInt(-1), Seq(Cmp(r1, expr2._1, GT)))
        case GreaterEquals => (ImmInt(-1), Seq(Cmp(r1, expr2._1, GE))) 
        case Less => (ImmInt(-1), Seq(Cmp(r1, expr2._1, LT)))
        case LessEquals => (ImmInt(-1), Seq(Cmp(r1, expr2._1, LE)))
        case Equal => (ImmInt(-1), Seq(Cmp(r1, expr2._1, EQ)))
        case NotEqual => (ImmInt(-1), Seq(Cmp(r1, expr2._1, NE)))
        case _ => {
          val out = regs.allocate
          instr += binaryOpToAssembly(out, r1, expr2._1)
          return (out, instr.toSeq)
        }
      }
    }
  }

  /* left expressions extending expressions and left values */
  sealed trait LExpr extends Expr with LValue {
    def pos: (Int, Int)
  }

  /* expressions extending left expressions */
  case class Ident(id: String)(val pos: (Int, Int)) extends LExpr

  case class ArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr
  
  case class IdentOrArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr

  /* expression parser bridges */
  object IntLiteral extends ParserBridgePos1[Int, IntLiteral]

  object CharLiteral extends ParserBridgePos1[Char, CharLiteral]

  object StrLiteral extends ParserBridgePos1[String, StrLiteral]

  object BoolLiteral extends ParserBridgePos1[Boolean, BoolLiteral]

  object Ident extends ParserBridgePos1[String, Ident]

  object ArrayElem extends ParserBridgePos2[String, List[Expr], ArrayElem]

  /* case class for a pair literal, (always null) */
  case class PairLiteralNull(val pos: (Int, Int)) extends Expr with ParserBridgePos0[Expr]

  /* identifier or array elements with parser bridge */
  object IdentOrArrayElem extends ParserBridgePos2[String, Option[List[Expr]], LExpr] {
    def apply(id: String, xs: Option[List[Expr]])(pos: (Int, Int)): LExpr = xs match {
      case None => Ident(id)(pos)
      case Some(xs) => ArrayElem(id, xs)(pos)
    }
  }

  /* operators */
  sealed trait UnaryOp

  case object Not extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
    def apply(x: Expr) = UnaryOpExpr(Not, x)(x.pos)
  }

  case object Negate extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
    def apply(x: Expr) = UnaryOpExpr(Negate, x)(x.pos)
  }

  case object Length extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
    def apply(x: Expr) = UnaryOpExpr(Length, x)(x.pos)
  }

  case object Ord extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
    def apply(x: Expr) = UnaryOpExpr(Ord, x)(x.pos)
  }

  case object Chr extends UnaryOp with ParserBridge1[Expr, UnaryOpExpr] {
    def apply(x: Expr) = UnaryOpExpr(Chr, x)(x.pos)
  }

  sealed trait BinaryOp

  case object Mul extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Mul, x, y)(x.pos, y.pos)
  }

  case object Div extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Div, x, y)(x.pos, y.pos)
  }

  case object Mod extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Mod, x, y)(x.pos, y.pos)
  }

  case object Add extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Add, x, y)(x.pos, y.pos)
  }

  case object Sub extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Sub, x, y)(x.pos, y.pos)
  }

  case object Greater extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Greater, x, y)(x.pos, y.pos)
  }

  case object GreaterEquals extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(GreaterEquals, x, y)(x.pos, y.pos)
  }

  case object Less extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Less, x, y)(x.pos, y.pos)
  }

  case object LessEquals extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(LessEquals, x, y)(x.pos, y.pos)
  }

  case object Equal extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Equal, x, y)(x.pos, y.pos)
  }

  case object NotEqual extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(NotEqual, x, y)(x.pos, y.pos)
  }

  case object And extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(And, x, y)(x.pos, y.pos)
  }

  case object Or extends BinaryOp with ParserBridge2[Expr, Expr, BinaryOpExpr] {
    def apply(x: Expr, y: Expr) = BinaryOpExpr(Or, x, y)(x.pos, y.pos)
  }

}
