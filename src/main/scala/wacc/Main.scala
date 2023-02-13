package wacc

object Main {

  import wacc.front.error.ErrorLogger

    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            ErrorLogger.err(Compiler.getUsage, -1)
        }

        val compiler = Compiler(args(0))
        if (!compiler.parse()) sys.exit(100)
        if (!compiler.typecheck) sys.exit(200)
        compiler.compile
    }

}

