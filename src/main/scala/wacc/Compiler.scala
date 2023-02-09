package wacc

import parsley.Parsley.{pure}
import java.io.File

class Compiler(private val file: File) {

    import Parser._
    import AST.Program
    import parsley.{Success, Failure}
    import error._
    import Errors.WACCError
    import parsley.combinator.skipMany
    import parsley.character.whitespace
    import parsley.errors.{ErrorBuilder, Token, TokenSpan}
    import parsley.errors.tokenextractors.{LexToken, TillNextWhitespace, MatchParserDemand}
    import parsley.Parsley
    import parsley.io._
    import Lexing.{IDENT, INTEGER, lexer, keywords}

    private var program: Option[Program] = None 

    def parse(): Boolean = {
        val pNode = Parser.program

        implicit val eb: ErrorBuilder[WACCError] = new WACCErrorBuilder with LexToken {
            private val idents = skipMany(whitespace) *> lexer.nonlexeme.names.identifier.map(x => s"identifier $x")

            private val ints = skipMany(whitespace) *> lexer.nonlexeme.numeric.integer.decimal32.map(x => s"integer $x")

            private val keywords_ = LexToken.constantSymbols((keywords.map(x => (skipMany(whitespace) *> lexer.nonlexeme.symbol(x), s"keyword $x")).toList): _*)

            def tokens: Seq[Parsley[String]] = idents +: ints +: keywords_

            override def extractItem(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = TillNextWhitespace.unexpectedToken(cs, amountOfInputParserWanted)

            
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
                ErrorLogger.err("cannot read file", 1)
            } 
        }
    }
    
    def typecheck: Boolean = program match {
        case Some(x) => {
            val errors = SemanticChecker.typecheck(x)
            if (errors.isEmpty) return true
            errors.foreach(println(_))
            return false
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