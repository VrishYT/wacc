package wacc

class Compiler {
    
    import java.nio.file.{Files, Paths}
    import scala.io.Source
    import Parser._
    import AST.Program
    import parsley.{Success, Failure}
    import error._

    private var filename = ""
    private var fileData = ""
    private var program: Option[Program] = None 
    private var exception: Option[CompilerException] = None

    override def toString: String = {
        exception match {
            case Some(x) => return x.toString
            case None => // TODO: AST Pretty Print???
        }
        return program.toString
    }

    def readTarget(): Boolean = {

        val path = Paths.get(filename)
        if (!Files.exists(path)) {
            val parentPath = path.getParent.toString
            val parent = parentPath.substring(parentPath.lastIndexOf("valid/") + 6) + "/"
            ErrorLogger.err(parent + path.getFileName + " not found", -1)
            return false
        }

        val builder = new StringBuilder()
        for (line <- Source.fromFile(filename).getLines()) {
            builder.append(line.trim)
            builder.append("\n")
        }
        fileData = builder.toString()
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
                ErrorLogger.err(x)
                false // should be unreachable
            } 
        }
    }
    
    def typecheck: Boolean = program match {
        case Some(x) => {
            SemanticChecker.typecheck(x)
            return true // should be unreachable for if semantically invalid 
        }
        case None => ErrorLogger.err("typecheck called before parse/readTarget", -1)
    }

    def compile = ???

}

object Compiler {

    def apply(targetFile: String): Compiler = {
        var c = new Compiler
        c.filename = targetFile
        c
    }

    def getUsage: String = {
        val sep = System.getProperty("line.separator")
        val s = new StringBuilder("Usage: ./compile [options] <target.wacc>")
        s.append(sep)
        s.append("   options:")
        s.append(sep)
        s.append("       -p, --only-parse        Parse only. Check the input file for syntax errors and generate an AST.")
        s.append(sep)
        s.append("       -s, --only-typecheck    Semantic check. Parse the file for syntax and semantic errors and generate an AST.")
        s.append(sep)
        s.append("       -c, --full-compile      Full Compilation (default). Run the full compilation process.")
        s.append(sep)
        s.append("       -t, --target            Target. Select target architecture (default arm32, options: x86-64-intel or x86-64).")
        s.append(sep)
        s.append("       -o, --optimise          Optimise. Run ARM Peephole optimisations over the generated assembly code.")
        s.append(sep)
        s.append("       -a, --print-assembly    View Assembly. Display ARM assembly code generated by the code generator.")
        s.append(sep)
        s.append("       -x, --execute           Execute. Assemble and Emulate the generated ARM code and display its output.")
        s.append(sep)
        s.append("       -d, --directory         Give directory of wacc files.")
        s.append(sep)
        s.append("       -h, --help              Show this message.")
        s.append(sep)
        s.append(sep)
        s.append(sep)
        s.append("   target.wacc: path to wacc program file to compile (or target directory if --directory option set)")
        return s.toString
    }
}