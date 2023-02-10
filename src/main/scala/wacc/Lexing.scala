package wacc

object Lexing{
    import parsley.Parsley
    import parsley.Parsley.{attempt, notFollowedBy}
    import parsley.implicits.character.{stringLift, charLift}
    import parsley.character.digit
    import parsley.token.{Lexer, predicate}
    import parsley.token.predicate.Basic
    import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc}
    import parsley.token.descriptions._
    import parsley.errors.combinator._

    /*set the lexer configurations for escape characters in WACC*/
    val escapeConfigs = text.EscapeDesc.plain.copy(escBegin = '\\',
                           literals = Set('\'', '\"', '\\'),
                           singleMap = Map( '0' -> 0x0000,
                                            'b' -> 0x0008,
                                            'f' -> 0x000c,
                                            'n' -> 0x000a,
                                            'r' -> 0x000d,
                                            't' -> 0x0009),
                           multiMap = Map.empty,
                           decimalEscape = text.NumericEscape.Illegal,
                           hexadecimalEscape = text.NumericEscape.Illegal,
                           octalEscape = text.NumericEscape.Illegal,
                           binaryEscape = text.NumericEscape.Illegal,
                           emptyEscape = None,
                           gapsSupported = false)

    /*set the lexer configurations for keywords in WACC, so that identifiers cannot be named the same as keywords*/
    val keywords = Set("begin", "null", "end", "is", "skip", "read", "free", 
                        "return", "exit", "print", "println", "if", "then", "else",
                        "fi", "while", "do", "done", "fst", "snd", "newpair", "call", 
                        "int", "bool", "char", "string", "pair", "true", "false", "len", "ord", "chr")

    private val desc = LexicalDesc.plain.copy(
        /*lexer configurations for names*/
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(c => Character.isLetter(c) || c == '_'),
            identifierLetter = Basic(c => Character.isLetterOrDigit(c) || c == '_')
        ),
        /*lexer configurations for symbols: operators / keywords*/
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = keywords,
            hardOperators = Set("*", "+", "-", "/", ">", ">=", "<", "<=", "==", "!=", "&&", "||"), 
            caseSensitive = false
        ),
        /*lexer configurations for signed integers*/
        numericDesc = numeric.NumericDesc.plain.copy(
            /* generic number*/
            integerNumbersCanBeHexadecimal = false,
            integerNumbersCanBeOctal = false,
            /* special literals */
            hexadecimalLeads = Set(),
            octalLeads = Set(),
            binaryLeads = Set(),
            /*exponents*/
            decimalExponentDesc = numeric.ExponentDesc.NoExponents,
            hexadecimalExponentDesc = numeric.ExponentDesc.NoExponents,
            octalExponentDesc = numeric.ExponentDesc.NoExponents,
            binaryExponentDesc = numeric.ExponentDesc.NoExponents
        ),
        /*lexer configuratons for strings and characters*/
        textDesc = text.TextDesc.plain.copy(
            escapeSequences = escapeConfigs,
            graphicCharacter = Basic((c) => c >= ' '.toInt && c != '\\' && c != '\'' && c != '\"')
        ),
        /*lexer configurations for whitespace and comments*/
        spaceDesc = SpaceDesc.plain.copy(
            commentLine = "#"
        )
    )


    val lexer = new Lexer(desc)

    /*definitions for identifier, integer, negate sign, string and character tokens*/

    val IDENT = lexer.lexeme.names.identifier
                                    .label("identifier")
                                    .explain("valid identifiers can only include \'_\' and alphanumeric characters, but also must not start with a digit")
    val INTEGER = lexer.lexeme.numeric.integer.decimal32
                                    
    val UNOP_MINUS = lexer.lexeme(attempt('-' *> notFollowedBy(digit)))

    val STR_LIT = lexer.lexeme.text.string.ascii
                                    .label("string literal")
                                    .explain("strings can only contain graphic ASCII characters")
    val CHR_LIT = lexer.lexeme.text.character.ascii
                                    .label("character literal")
                                    .explain("a character must be graphic ASCII")

    def fully[A](p: Parsley[A]) = lexer.fully(p)

    /*definition of implicitis so that keywords are recognised as separate tokens within parsley*/
    val implicits = lexer.lexeme.symbol.implicits
}
 