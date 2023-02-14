package wacc.back

sealed trait Instruction 

/* Arithmetic Instructions */
case class Sub(dest: Register, src: Register) extends Instruction {
    override def toString(): String = s"sub $dest, $src"
}
case class Add(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"add $dest, $src, $op"
}
case class Mul(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"mul $dest, $src, $op"
}

/* Compare Instructions */
case class Cmp(src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"cmp $src, $op"
}

/* Logical Instructions */
case class And(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"and $dest, $src, $op"
}
case class Xor(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"eor $dest, $src, $op"
}
case class Or(dest: Register, src: Register, op: Operand) extends Instruction {
    override def toString(): String = s"orr $dest, $src, $op"
}

/* Loading and storing Instructions */
case class Mov(dest: Register, op: Operand) extends Instruction {
    override def toString(): String = s"mov $dest, $op"
}
case class Load(dest: Register, src: Register) extends Instruction {
    override def toString(): String = s"ldr $dest, $src"
}
case class Store(dest: Register, src: Register) extends Instruction {
    override def toString(): String = s"str $dest, $src"
}

/* Branch Instructions */
case class Branch(label: String) extends Instruction {
    override def toString(): String = s"b $label"
}
case class LinkBranch(label: String) extends Instruction {
    override def toString(): String = s"bl $label"
}

/* Stack Instruction */
case class Push(src: Register*) extends Instruction {
    override def toString(): String = s"push {${src.mkString(", ")}}"
}
case class Pop(dest: Register*) extends Instruction {
    override def toString(): String = s"pop {${dest.mkString(", ")}}"
}

case class Label(label: String) extends Instruction {
    override def toString(): String = s"label $label"
}
case class Directive(name: String) extends Instruction {
    override def toString(): String = s"directive $name"
}