package wacc

import parsley.Parsley.{pure}
import java.io.File

class Compiler(private val file: File) {

    import Parser._
    import AST.Program
    import parsley.{Success, Failure}
    import error._
    import error.Errors.WACCError
    import parsley.combinator.skipMany
    import parsley.character.whitespace
    import parsley.errors.{tokenextractors, ErrorBuilder, Token, TokenSpan}
    import parsley.errors.tokenextractors.LexToken.constantSymbols
    import parsley.Parsley
    import parsley.io._
    import Lexing.{IDENT, INTEGER, lexer, keywords}

    private var program: Option[Program] = None 

    def parse(): Boolean = {
        val pNode = Parser.program
        implicit val eb: ErrorBuilder[WACCError] = new WACCErrorBuilder with tokenextractors.LexToken {
            private val idents = lexer.nonlexeme.names.identifier.map(x => s"identifier $x")

            private val ints = lexer.nonlexeme.numeric.integer.decimal32.map(x => s"integer $x")

            private val keywords_ = constantSymbols((keywords.map(x => (lexer.nonlexeme.symbol(x), s"keyword $x")).toList): _*)
            
            def tokens: Seq[Parsley[String]] = idents +: ints +: keywords_
        }

        val result = pNode.parseFromFile(file)
        result match {
            case util.Success(x) => x match {
                case Success(x) => {
                    program = Some(x)
                    true
                }
                case x: Failure[WACCError] => {
                    ErrorLogger.err(x)                    
                }
            }
            case x: util.Failure[_] => {
                ErrorLogger.err("cannot read file")
                false // should be unreachable
            } 
        }
    }
    
    def typecheck: Boolean = program match {
        case Some(x) => {
            SemanticChecker.typecheck(x)
            return true
        }
        case None => ErrorLogger.err("typecheck called before parse/readTarget", -1)
    }

    def compile = ???

}

object Compiler {

    def apply(targetFile: String): Compiler = new Compiler(new File(targetFile))

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