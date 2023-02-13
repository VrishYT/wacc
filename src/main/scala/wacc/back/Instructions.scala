package wacc.back

sealed trait Instruction 

/* Arithmetic Instructions */
case class Sub(dest: Register, src: Register) extends Instruction
case class Add(dest: Register, src: Register, op: Operand) extends Instruction
case class Mul(dest: Register, src: Register, op: Operand) extends Instruction

/* Compare Instructons */
case class Cmp(src: Register, op: Operand) extends Instruction

/* Logical Instructions */
case class And(dest: Register, src: Register, op: Operand) extends Instruction
case class Xor(dest: Register, src: Register, op: Operand) extends Instruction
case class Or(dest: Register, src: Register, op: Operand) extends Instruction

/* Loading and storing Instructions */
case class Mov(dest: Register, op: Operand) extends Instruction
case class Load(dest: Register, src: Register) extends Instruction
case class Store(dest: Register, src: Register) extends Instruction

/* Branch Instructions */
case class Branch(label: String) extends Instruction
case class LinkBranch(label: String) extends Instruction

/* Stack Instrcution */
case class Push(src: Register) extends Instruction
case class Pop(dest: Register) extends Instruction

case class Label(label: String) extends Instruction
case class Directive(name: String) extends Instruction