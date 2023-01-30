package wacc

object Parser{
    import parsley.combinator.{sepBy, sepBy1}

    import Lexing._
    import implicits.implicitSymbol
    
    // import ast._

    val BOOL_LIT = "true" <|> "false"
    val PAIR_LIT = "null"
    val PAIR_ELEM = "fst" <|> "snd"
    val BASE_TYPE = "int" <|> "bool" <|> "char"  <|> "string"
    val UNARY_OP = "!" <|> UNOP_MINUS <|> "len" <|> "ord" <|> "chr"  // Not sure if these are tokenised already by lexer
    val BINARY_OP = "*" <|> "/" <|>  "%" <|>  "+" <|>  "-" <|>  ">" <|>  ">=" <|>  "<" <|>  "<=" <|>  "==" <|>  "!=" <|>  "&&" <|> "||" // Not sure if these are tokenised already
    val ARRAY_ELEM = IDENT *> "[" *> sepBy(EXPR, "][") <* "]" // Not sure about this one
    val PAIR_TYPE = "pair" *> "(" *> sepBy1(PAIR_ELEM_TYPE, ",") <* ")"
    val ARRAY_TYPE = (BASE_TYPE <|> PAIR_TYPE) <* "[]"           // should be TYPE but array makes it recursive
    val TYPE = BASE_TYPE <|> ARRAY_TYPE <|> PAIR_TYPE

    val PAIR_ELEM_TYPE = BASE_TYPE | "pair"  // needs ARRAY_TYPE too but is recursive??


    
    //lazy val EXPR: Parsley[Expr] = 

    
}


 