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

    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            // Unicode is also possible instead of Basic
            identifierStart = Basic(c => Character.isLetter(c) || c == '_'),
            identifierLetter = Basic(c => Character.isLetterOrDigit(c) || c == '_')
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("begin", "null", "end", "is", "skip", "read", "free", 
                                "return", "exit", "print", "println", "if", "then", "else",
                                "fi", "while", "do", "done", "fst", "snd", "newpair", "call", 
                                "int", "bool", "char", "string", "pair", "true", "false"),
            hardOperators = Set("*", "+", "-", "/", ">", ">=", "<", "<=", "==", "!=", "&&", "||", "len", "ord", "chr"),
            //TODO: check whether len ord chr should be in keywords or operators 
            caseSensitive = false
        ),
        numericDesc = numeric.NumericDesc.plain.copy(
            // generic number
            integerNumbersCanBeHexadecimal = false,
            integerNumbersCanBeOctal = false,
            // special literals
            hexadecimalLeads = Set(),
            octalLeads = Set(),
            binaryLeads = Set(),
            // exponents
            decimalExponentDesc = numeric.ExponentDesc.NoExponents,
            hexadecimalExponentDesc = numeric.ExponentDesc.NoExponents,
            octalExponentDesc = numeric.ExponentDesc.NoExponents,
            binaryExponentDesc = numeric.ExponentDesc.NoExponents
        ),
        textDesc = text.TextDesc.plain.copy(
            escapeSequences = escapeConfigs,
            graphicCharacter = Basic((c) => c >= ' '.toInt && c != '\\' && c != '\'' && c != '\"')
        ),
        spaceDesc = SpaceDesc.plain.copy(
            commentLine = "#"
        )
    )


    val lexer = new Lexer(desc)

    val IDENT = lexer.lexeme.names.identifier
                                    .label("identifier")
                                    .explain("valid identifiers can only include \'_\' and alphanumeric characters, but also must not start with a digit")
    val INTEGER = lexer.lexeme.numeric.integer.decimal32
                                    .label("number")
                                    .explain("all numbers are signed 32-bit integers")
    val UNOP_MINUS = lexer.lexeme(attempt('-' *> notFollowedBy(digit)))

    val STR_LIT = lexer.lexeme.text.string.ascii
                                    .label("string")
                                    .explain("strings can only contain graphic ASCII characters")
    val CHR_LIT = lexer.lexeme.text.character.ascii
                                    .label("character")
                                    .explain("a character must be graphic ASCII")

    def fully[A](p: Parsley[A]) = lexer.fully(p)
    val implicits = lexer.lexeme.symbol.implicits
}
 