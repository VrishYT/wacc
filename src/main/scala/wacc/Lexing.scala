object Lexing{
    import parsley.Parsley
    import parsley.token.{Lexer, predicate}
    import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc}
    import parsley.token.predicate.{Unicode}
    import parsley.token.descriptions._

    val escapeConfigs = text.EscapeDesc(escBegin = '\\',
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
            identifierStart = predicate.Basic(_.isLetter),
            identifierLetter = predicate.Basic(_.isLetterOrDigit)
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("negate", "begin", "end", "is", "skip", "read", "free", 
                                "return", "exit", "print", "println", "if", "then", "else",
                                "fi", "while", "do", "done", "fst", "snd", "newpair", "call", 
                                "int", "bool", "char", "string", "pair","true", "false", "=", "(", ")", "[", "]", ";", "," ),
            hardOperators = Set("*", "+", "-", "/", ">", ">=", "<", "<=", "==", "!=", "&&", "||", "len", "ord", "chr"),
            //TODO: check whether len ord chr should be in keywords or operators 
            caseSensitive = false
        ),
        numericDesc = numeric.NumericDesc(
            literalBreakChar = numeric.BreakCharDesc.NoBreakChar,
            leadingDotAllowed = false,
            trailingDotAllowed = false,
            leadingZerosAllowed = true,
            positiveSign = numeric.PlusSignPresence.Optional,
            // generic number
            integerNumbersCanBeHexadecimal = false,
            integerNumbersCanBeOctal = false,
            integerNumbersCanBeBinary = false,
            realNumbersCanBeHexadecimal = false,
            realNumbersCanBeOctal = false,
            realNumbersCanBeBinary = false,
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
        textDesc = text.TextDesc(
            escapeSequences = escapeConfigs,
            characterLiteralEnd = '\'',
            stringEnds = Set("\""),
            multiStringEnds = Set.empty,
            graphicCharacter = Unicode(_ >= ' '.toInt)
        ),
        spaceDesc = SpaceDesc(
            commentStart = "",
            commentEnd = "",
            commentLine = "#",
            commentLineAllowsEOF = true,
            nestedComments = false,
            space = Unicode(Character.isWhitespace),
            whitespaceIsContextDependent = false
        )
    )


    private val lexer = new Lexer(desc)

    val identifier = lexer.lexeme.names.identifier
    val number = lexer.lexeme.numeric.natural.decimal

    def fully[A](p: Parsley[A]) = lexer.fully(p)
    val implicits = lexer.lexeme.symbol.implicits
}
