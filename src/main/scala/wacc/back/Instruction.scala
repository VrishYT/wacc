package wacc
package back

import Condition._

sealed trait Instruction {
    def arm11(implicit sb: StringBuilder): Unit
}

/* Arithmetic Instructions */
/* dest = src - op */
case class Sub(dest: Register, src: Register, op: Operand) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tsubs $dest, $src, $op")
        sb.append("\n")
    }
}
/* dest = src + op */
case class Add(dest: Register, src: Register, op: Operand) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tadds $dest, $src, $op")
        sb.append("\n")
    }
}
/* dest = src * op */
case class Mul(dest: Register, src: Register, op: Operand) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tmul $dest, $src, $op")
        sb.append("\n")
    }
}

case class SMull(dstHi: Register, dstLow: Register, reg1: Register, reg2: Register) extends Instruction{
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tsmull $dstHi, $dstLow, $reg1, $reg2")
        sb.append("\n")
    }
}

case object DivMod extends Instruction{
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append("\tbl __aeabi_idivmod")
        sb.append("\n")
    }
}

/* Compare Instructions */
case class Cmp(src: Register, op: Operand) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tcmp $src, $op")
        sb.append("\n")
    }
}

/* Logical Instructions */
case class And(dest: Register, src: Register, op: Operand) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tand $dest, $src, $op")
        sb.append("\n")
    }
}
case class Xor(dest: Register, src: Register, op: Operand) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\teor $dest, $src, $op")
        sb.append("\n")
    }
}
case class Or(dest: Register, src: Register, op: Operand) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\torr $dest, $src, $op")
        sb.append("\n")
    }
}

case class Len(dest: Register, op: Operand) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tmov $dest, $op")
        sb.append("\n")
    }
}

/* Loading and storing Instructions */
case class Mov(dest: Register, op: Operand, cond: Condition = AL) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tmov${cond.toString.toLowerCase()} $dest, $op")
        sb.append("\n")
    }
}
case class Load(dest: Register, src: Operand, signedByte: Boolean = false) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        if (signedByte){
            sb.append(s"\tldrsb $dest, $src")
            sb.append("\n")
        }else {
            sb.append(s"\tldr $dest, $src")
            sb.append("\n")
        }
    }
}
case class Store(dest: Register, src: Operand, postAddress: Boolean = false, byte: Boolean = false) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        if (byte && postAddress) {
            sb.append(s"\tstrb $dest, $src!")
            sb.append("\n")
        } else if (byte) {
            sb.append(s"\tstrb $dest, $src")
            sb.append("\n")
        } else {
            sb.append(s"\tstr $dest, $src")
            sb.append("\n")
        }
    } 
}
case class StoreB(dest: Register, src: Operand) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tstrb $dest, $src")
        sb.append("\n")
    }
}
/* Branch Instructions */
case class Branch(label: String, cond: Condition = AL) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tb${cond.toString.toLowerCase()} $label")
        sb.append("\n")
    }
}
case class LinkBranch(label: String, cond: Condition = AL) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tbl${cond.toString.toLowerCase()} $label")
        sb.append("\n")
    }
}

/* Stack Instruction */
case class Push(src: Register*) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tpush {${src.mkString(", ")}}")
        sb.append("\n")
    }
}


case class Pop(dest: Register*) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\tpop {${dest.mkString(", ")}}")
        sb.append("\n")
    }
}

/*Data and Text Instructions*/
case class Label(label: String) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"${label}:")
        sb.append("\n")
    }
}
case class Section(section: String) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(section)
        sb.append("\n")
    }
}
case class Directive(directive: String) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"\t$directive")
        sb.append("\n")
    }
}
case class Comment(comment: String) extends Instruction {
    override def arm11(implicit sb : StringBuilder) : Unit = {
        sb.append(s"@ ${comment}")
        sb.append("\n")
    }
}