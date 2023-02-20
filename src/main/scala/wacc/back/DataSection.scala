package wacc.back

import scala.collection.mutable.{Map => MapM}
import wacc.back._

sealed abstract class Data() {
    
}

//sealed abstract class PrintSection(val type: String) extends Data
case object PrintCharSection {
    override def toString(): String = {
        return ".data\n" +
	            ".word 2\n" + 
                ".L._printc_char0:\n" +
	            ".asciz \"%c\"\n" +
                ".text\n\n" +
                "_printc:\n" + 
	            "push {lr}\n" +
	            "ldr r0, =.L._printc_char0\n" + 
	            "bl printf\n" +
	            "mov r0, #0\n" +
	            "bl fflush\n" +
	            "pop {pc}\n"
    }
}

case object PrintIntSection {
    override def toString(): String = {
        return ".data\n" +
	            ".word 2\n" + 
                ".L._printi_int0:\n" +
	            ".asciz \"%d\"\n" +
                ".text\n\n" +
                "_printi:\n" + 
	            "push {lr}\n" +
	            "ldr r0, =.L._printi_int0\n" + 
	            "bl printf\n" +
	            "mov r0, #0\n" +
	            "bl fflush\n" +
	            "pop {pc}\n"
    }
}

case object PrintStringSection {
    override def toString(): String = {
        return ".data\n" +
	            ".word 4\n" + 
                ".L._prints_str0:\n" +
	            ".asciz \"%.*s\"\n" +
                ".text\n\n" +
                "_prints:\n" + 
	            "push {lr}\n" +
	            "ldr r0, =.L._prints_str0\n" + 
	            "bl printf\n" +
	            "mov r0, #0\n" +
	            "bl fflush\n" +
	            "pop {pc}\n"
    }
}

case object PrintBoolSection {
    override def toString(): String = {
        ".data\n" + 
        ".word 5\n" + 
        ".L._printb_str0:\n" + 
        ".asciz \"false\"\n" + 
        ".word 4\n" + 
        ".L._printb_str1:\n" + 
        ".asciz \"true\"\n" + 
        ".word 4\n" + 
        ".L._printb_str2:\n" + 
        ".asciz \"%.*s\"\n" + 
        ".text\n\n" + 
        "_printb:\n" + 
        "push {lr}\n" + 
        "cmp r0, #0\n" + 
        "bne .L_printb0\n" + 
        "ldr r2, =.L._printb_str0\n" + 
        "b .L_printb1\n" + 
        ".L_printb0:\n" + 
        "ldr r2, =.L._printb_str1\n" + 
        ".L_printb1:\n" + 
        "ldr r1, [r2, #-4]\n" + 
        "ldr r0, =.L._printb_str2\n" + 
        "bl printf\n" + 
        "mov r0, #0\n" + 
        "bl fflush\n" + 
        "pop {pc}\n"          
    }
}

case object PrintNewLine {
    override def toString(): String = {
        return "\n.data\n" +
	            ".word 2\n" + 
                ".L._println_ln0:\n" +
	            ".asciz \"\\n\"" +
                ".text\n\n" +
                "_println:\n" + 
	            "push {lr}\n" +
	            "ldr r0, =.L._println_ln0\n" + 
	            "bl printf\n" +
	            "mov r0, #0" +
	            "bl fflush" +
	            "pop {pc}"
    }
}


class TextSection extends Data{

    // TODO: change key type to label ???
    private val table = MapM[String, String]()
    private var counter = 0

    def add(value: String): String = {
        val label = ".L.str" + counter
        table(label) = value
        counter += 1
        return label
    }

    def get(id: String): String = table.get(id) match {
        case Some(x) => x
        case None => ???
    }

    override def toString(): String = {
        if (table.isEmpty) return ""

        // TODO: update according to other TODO's related to DataSection
        def entryToAssembly(entry: (String, String)): String = {
            val label = entry._1
            val data = entry._2

            return s"\t.word ${data.length()}\n" + 
                   label + ":\n" +
                   s"\t.asciz \"${data}\""
        }

        return ".data\n" + 
               table.toSeq.map(entryToAssembly).mkString +
               ".text\n"
    }
    
}
