package wacc
package ast

import wacc.front.ParserBridge._
import wacc.back._
import scala.collection.mutable.ListBuffer

/* left expressions extending expressions and left values */
sealed trait LExpr extends Expr with LValue {
  def pos: (Int, Int)
  override def toAssembly(gen: CodeGenerator, table: Table): RegAssembly = TODOAssembly
}

/* expressions extending left expressions */
case class Ident(id: String)(val pos: (Int, Int)) extends LExpr {
  override def toAssembly(gen: CodeGenerator, table: Table): RegAssembly = RegAssembly(gen.regs.get(id))
}

object Ident extends ParserBridgePos1[String, Ident]

case class ArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr {

  override def toAssembly(gen: CodeGenerator, table: Table): RegAssembly = {
    return toAssemblyLoad(gen, table)
    // val out = xs.map(_.toAssembly(gen, table))
    // val func = Func.callFunction(id, xs)
  }

  def toAssemblyLoad(gen: CodeGenerator, table: Table): RegAssembly = {
    val outAss = gen.regs.allocate
    val outReg = outAss.getReg

    // val ptrAss = gen.regs.allocate // R3
    val ptrReg = Register(3)//ptrAss.getReg

    // val indexAss = gen.regs.allocate // R10
    val indexReg = Register(10)//indexAss.getReg

    val arrAss = RegAssembly(gen.regs.get(id))
    val arrayOp = arrAss.getOp

    val first = xs.head
    val rest = xs.tail

    val firstAss = first.toAssembly(gen, table)
    val firstOp = firstAss.getOp

    val accAss = gen.regs.allocate
    val accReg = accAss.getReg

    gen.postSections.addOne(ArrayBoundsCheck)
    gen.postSections.addOne(ArrayLoadSection)

    val startInstrs: Seq[Instruction] = /*indexAss.instr ++ ptrAss.instr ++*/ outAss.instr ++ arrAss.instr ++ firstAss.instr ++ Seq(
      Mov(indexReg, firstOp),
      Mov(ptrReg, arrayOp),
      LinkBranch("_arrload"), // to define
      Mov(accReg, ptrReg)
    )

    val restInstrs: Seq[Instruction] = rest.map(x => {
      val xAss = x.toAssembly(gen, table)
      val op = xAss.getOp
      xAss.instr ++ Seq(
        Push(accReg),
        Mov(indexReg, op),
        Pop(accReg),
        Mov(ptrReg, accReg),
        LinkBranch("_arrload"), // to define
        Mov(accReg, ptrReg)
      )
    }).flatten

    val finalInstrs = Seq(Mov(outReg, accReg))

    // val comment: String = s"START: ${startInstrs}, REST: ${restInstrs}, FINAL: ${finalInstrs}"

    return RegAssembly(outReg, startInstrs ++ restInstrs ++ finalInstrs /*++ Seq(Comment(comment))*/)
  }
  
  
  def toAssemblyStore(gen: CodeGenerator, table: Table, rhs: Assembly): RegAssembly = {

    // val ptrAss = gen.regs.allocate // R3
    val ptrReg = Register(3)//ptrAss.getReg

    // val indexAss = gen.regs.allocate // R10
    val indexReg = Register(10)//indexAss.getReg
    
    // val valAss = gen.regs.allocate // R8
    val valReg = Register(8)//valAss.getReg

    val push = Seq(Push(Register(3), Register(8), Register(10)))
    val pop = Seq(Pop(Register(3), Register(8), Register(10)))

    // val arrOp = gen.regs.get(id)

    // if (arrOp.i == 1 )
    val arrAss = RegAssembly(gen.regs.get(id))
    val arrayOp = arrAss.getOp

    val accAss = gen.regs.allocate
    val accReg = accAss.getReg

    val last: Expr = xs.last
    val lastAss = last.toAssembly(gen, table)
    val lastOp = lastAss.getOp

    gen.postSections.addOne(ArrayBoundsCheck)
    gen.postSections.addOne(ArrayLoadSection)
    gen.postSections.addOne(ArrayStoreSection)

    
    val _arrStore: Seq[Instruction] = Seq(
      Mov(indexReg, lastOp),
      Mov(valReg, rhs.getOp),
      Mov(ptrReg, arrayOp),
      LinkBranch("_arrStore")
    )
    
    val initList: List[Expr] = xs.init
    if (initList.isEmpty) {
      return RegAssembly(Register(0), push ++ lastAss.instr ++ _arrStore ++ pop)
    }

    val first: Expr = initList.head
    val firstAss = first.toAssembly(gen, table)
    val firstOp = firstAss.getOp

    def _arrLoad(op: Operand, instr: Seq[Instruction]): Seq[Instruction] = {
      Seq(Mov(indexReg, op)) ++ instr ++ Seq(
        Mov(ptrReg, arrayOp),
        LinkBranch("_arrLoad"),
        Mov(accReg, ptrReg),
        Push(accReg)
      )
    }

    val middleList: List[Expr] = initList.tail
    if (middleList.isEmpty) {
      // val pop = Pop()
      return RegAssembly(Register(0),
        push ++ firstAss.instr ++ _arrLoad(firstOp, Seq()) ++
        lastAss.instr ++ _arrStore ++ pop
      )
    }
    
    val middleInstr: Seq[Instruction] = initList.map(x => {
      val xAss = x.toAssembly(gen, table)
      val xOp = xAss.getOp
      xAss.instr ++ _arrLoad(xOp, Seq(Pop(accReg)))
    }).flatten

    return RegAssembly(Register(0),
      push ++ firstAss.instr ++ _arrLoad(firstOp, Seq()) ++
      middleInstr ++
      lastAss.instr ++ _arrStore ++ pop
    )
    
  }

  
}
  
object ArrayElem extends ParserBridgePos2[String, List[Expr], ArrayElem]
  
case class IdentOrArrayElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LExpr

/* identifier or array elements with parser bridge */
object IdentOrArrayElem extends ParserBridgePos2[String, Option[List[Expr]], LExpr] {
    def apply(id: String, xs: Option[List[Expr]])(pos: (Int, Int)): LExpr = xs match {
      case None => Ident(id)(pos)
      case Some(xs) => ArrayElem(id, xs)(pos)
    }
}



