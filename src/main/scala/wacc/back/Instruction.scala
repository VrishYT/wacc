package wacc
package back

import Condition._

sealed trait Instruction 

/* Arithmetic Instructions */
/* dest = src - op */
case class Sub(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"\tsub $dest, $src, $op"
}
/* dest = src + op */
case class Add(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"\tadd $dest, $src, $op"
}
/* dest = src * op */
case class Mul(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"\tmul $dest, $src, $op"
}
case class Div(dest: Register, src: Register, op: Operand) extends Instruction

/* Compare Instructions */
case class Cmp(src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"\tcmp $src, $op"
}

/* Logical Instructions */
case class And(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"\tand $dest, $src, $op"
}
case class Xor(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"\teor $dest, $src, $op"
}
case class Or(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"\torr $dest, $src, $op"
}

case class Len(dest: Register, op: Operand) extends Instruction {
    override def toString(): String = s"\tmov $dest, $op"
}

/* Loading and storing Instructions */
case class Mov(dest: Register, op: Operand) extends Instruction {
    override def toString(): String = s"\tmov $dest, $op"
}
case class Load(dest: Register, src: Operand) extends Instruction {
    override def toString(): String = s"\tldr $dest, $src"
}
case class Store(dest: Register, src: Operand) extends Instruction {
    override def toString(): String = s"\tstr $dest, $src"
}

/* Branch Instructions */
case class Branch(label: String, cond: Condition = AL) extends Instruction {
    override def toString(): String = s"\tb${cond.toString.toLowerCase()} $label"
}
case class LinkBranch(label: String) extends Instruction {
    override def toString(): String = s"\tbl $label"
}

/* Stack Instruction */
case class Push(src: Register*) extends Instruction {
    override def toString(): String = s"\tpush {${src.mkString(", ")}}"
}
case class Pop(dest: Register*) extends Instruction {
    override def toString(): String = s"\tpop {${dest.mkString(", ")}}"
}

case class Label(label: String) extends Instruction {
    override def toString(): String = s"${label}:"
}
case class Section(section: String) extends Instruction {
    override def toString(): String = section
}