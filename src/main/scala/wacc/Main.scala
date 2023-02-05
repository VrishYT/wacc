package wacc

object Main {

    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            Compiler.printUsage
            sys.exit(-1)
        }

        val compiler = Compiler(args(0))
        compiler.readTarget()
        if (!compiler.parse) sys.exit(100)
        // compiler.typecheck
        // compiler.compile
    }

}

