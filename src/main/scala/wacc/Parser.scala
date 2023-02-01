package wacc
import parsley.Parsley
object Parser{
    import parsley.combinator.{sepBy, sepBy1, attemptChoice}
    import parsley.character.{string, strings}
    import parsley.expr.{precedence, Ops, InfixL, Prefix}
    import Lexing.lexer
    import Lexing._
    import implicits.implicitSymbol
    import parsley.token.Lexer
    import AST._

    // sealed trait Boolean 
    // case object True extends Boolean
    // case object False extends Boolean

    val BOOL_LIT = lexer.lexeme.symbol("true") #> true <|> 
                   lexer.lexeme.symbol("false") #> false
                                 
    val PAIR_LIT = lexer.lexeme.symbol("null") #> PairLiteralNull

    val BASE_TYPE = lexer.lexeme.symbol("int") #> IntType <|>
                    lexer.lexeme.symbol("string") #> StringType <|>
                    lexer.lexeme.symbol("bool") #> BoolType <|> 
                    lexer.lexeme.symbol("char") #> CharType
    
    private lazy val atom: Parsley[Expr] = 
                    "(" *> expr <* ")" <|> IntLiteral(INTEGER) <|> CharLiteral(CHR_LIT) <|>
                    StrLiteral(STR_LIT) <|> BoolLiteral(BOOL_LIT) <|> Ident(IDENT) <|> PAIR_LIT

    val expr: Parsley[Expr] = precedence[Expr](
        atom)(
                      Ops(Prefix)(Length <# "len", Ord <# "ord", Chr <# "chr", Negate <# UNOP_MINUS, Not <# "!"),
                      Ops(InfixL)(Mul <# "*", Div <# "/", Mod <# "%"),
                      Ops(InfixL)(Add <# "+", Sub <# "-"),
                      Ops(InfixL)(Greater <# ">", GreaterEquals <# ">=", Less <# "<", LessEquals <# "<="),
                      Ops(InfixL)(Equal <# "==", NotEqual <# "!="),
                      Ops(InfixL)(And <# "&&"),
                      Ops(InfixL)(Or <# "||")
                   )



    // val PAIR_ELEM = attemptChoice("fst", "snd")
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


 