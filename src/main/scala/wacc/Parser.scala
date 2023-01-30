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
    val UNARY_OP = "!" <|> UNOP_MINUS <|> "len" <|> "ord" <|> "chr"
    
    //lazy val EXPR: Parsley[Expr] = 

    
}


 