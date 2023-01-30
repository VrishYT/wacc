package wacc

object Main {

    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            Compiler.printUsage
            sys.exit(-1)
        }

        val compiler = Compiler(args(0))
        compiler.readTarget()
        return compiler.compile
    }

}

