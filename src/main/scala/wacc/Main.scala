package wacc

object Main {

    import error._

    def main(args: Array[String]): Unit = {
        try {
            if (args.length == 0) {
                ErrorLogger.err(Compiler.getUsage, -1)
            }

            val compiler = Compiler(args(0))
            compiler.parse
            if (!compiler.typecheck) {
                sys.exit(200)
            }
            // compiler.compile
        } catch {
            case x: CompilerException => x.quit
        }
    }

}

