package wacc.error

import parsley.position._

class CompilerException(private val message: String, private val exit: Int) extends Exception(message) {
    // override def toString(): String = message
    def quit = {
        println(this)
        // sys.exit(exit)
    }
}

object CompilerException {
    def apply(msg: String, exit: Int): CompilerException = new CompilerException(msg, exit)
}