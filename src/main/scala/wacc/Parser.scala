package wacc

object Parser{
    import parsley.combinator.{sepBy, sepBy1}

    import Lexing._
    import implicits.implicitSymbol
    
    // import ast._

    val BOOL_LIT = "true" <|> "false"
    val PAIR_LIT = "null"
    
    //lazy val EXPR: Parsley[Expr] = 

    
}


 