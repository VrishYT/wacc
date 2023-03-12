package wacc
package ast

import parsley.genericbridges._

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

case class ClassType(class_id: String) extends Type

object ClassType extends ParserBridge1[String, ClassType]

/* base types extending type */
sealed trait BaseType extends Type

case object NoType extends BaseType with ParserBridge0[BaseType] {
  def apply() = NoType
}

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