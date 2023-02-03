package wacc

class Compiler {
    
    import java.nio.file.{Files, Paths}
    import scala.io.Source
    import Parser._
    import AST.Program
    import parsley.{Success, Failure}

    private var filename = ""
    private var fileData = ""
    private var program: Option[Program] = None 

    def readTarget(): Boolean = {

        val path = Paths.get(filename)
        if (!Files.exists(path)) {
            sys.error("File '" + filename + "' not found")
            return false
        }

        val builder = new StringBuilder()
        for (line <- Source.fromFile(filename).getLines()) {
            if (!line.trim.startsWith("#")) {
                builder.append(line.trim)
                builder.append("\n")
            }
        }
        fileData = builder.toString().trim
        return true
    }

    def parse(): Boolean = {
        val pNode = Parser.program
        val result = pNode.parse(fileData)
        result match {
            case Success(x) => {
                program = Some(x)
                true
            }
            case x: Failure[_] => {
                println(x)
                false
            } 
        }
    }
    
    def typecheck = true

    def compile = ???

}

object Compiler {

    def apply(targetFile: String): Compiler = {
        var c = new Compiler
        c.filename = targetFile
        c
    }

    def printUsage = {
        println("Usage: ./compile [options] <target.wacc>")
        println("   options:")
        println("       -p, --only-parse        Parse only. Check the input file for syntax errors and generate an AST.")
        println("       -s, --only-typecheck    Semantic check. Parse the file for syntax and semantic errors and generate an AST.")
        println("       -c, --full-compile      Full Compilation (default). Run the full compilation process.")
        println("       -t, --target            Target. Select target architecture (default arm32, options: x86-64-intel or x86-64).")
        println("       -o, --optimise          Optimise. Run ARM Peephole optimisations over the generated assembly code.")
        println("       -a, --print-assembly    View Assembly. Display ARM assembly code generated by the code generator.")
        println("       -x, --execute           Execute. Assemble and Emulate the generated ARM code and display its output.")
        println("       -d, --directory         Give directory of wacc files.")
        println("       -h, --help              Show this message.")
        println()
        println("   target.wacc: path to wacc program file to compile (or target directory if --directory option set)")
    }
}