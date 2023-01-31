package wacc

object Parser{
    import parsley.combinator.{sepBy, sepBy1, attemptChoice}
    import parsley.character.{string, strings}
    import Lexing.lexer
    import Lexing._
    import implicits.implicitSymbol
    import parsley.token.Lexer
    import AST._


    //case class VarId(v: String) extends Expr

    sealed trait Boolean 
    case object True extends Boolean
    case object False extends Boolean

    val BOOL_LIT = attemptChoice(lexer.lexeme.symbol("true") #> True, lexer.lexeme.symbol("false") #> False)
    val PAIR_LIT = lexer.lexeme.symbol("null") #> Null
    val PAIR_ELEM = strings("fst", "snd")
    val BASE_TYPE = attemptChoice(lexer.lexeme.symbol("int") #> IntType, lexer.lexeme.symbol("string") #> StringType, lexer.lexeme.symbol("bool") #> BoolType, lexer.lexeme.symbol("char") #> CharType)
    val UNARY_OP = attemptChoice("!", UNOP_MINUS, "len", "ord", "chr")  // Not sure if these are tokenised already by lexer
    val BINARY_OP = strings("*", "/",  "%",  "+",  "-",  ">",  ">=",  "<",  "<=",  "==",  "!=",  "&&", "||") // Not sure if these are tokenised already
    
    
    // private val `<literal>` = CharLiteral(CHR_LIT) <|> StrLiteral(STR_LIT) <|> IntLiteral(INTEGER) <|> BoolLiteral(BOOL_LIT)
    // private val `<var-id>` = VarId(IDENT)

     

     
    // lazy val ARRAY_ELEM = IDENT *> "[" *> sepBy1(EXPR, "][") <* "]" // Not sure about this one
    // val ARRAY_LITER = "[" *> ((sepBy1(EXPR, ",") <* "]") <|> "]") 
    // val PAIR_TYPE = "pair" *> "(" *> sepBy1(PAIR_ELEM_TYPE, ",") <* ")"
    // lazy val ARRAY_TYPE = TYPE <* "[]"           // should be TYPE but array makes it recursive
    // val TYPE = BASE_TYPE <|> ARRAY_TYPE <|> PAIR_TYPE

    // val PAIR_ELEM_TYPE = BASE_TYPE <|> ARRAY_TYPE <|> "pair"  // needs ARRAY_TYPE too but is recursive??


    
    //lazy val EXPR: Parsley[Expr] = 

    
}


 