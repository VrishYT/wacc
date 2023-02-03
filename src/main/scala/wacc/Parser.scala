package wacc
import parsley.Parsley
import parsley.Parsley.attempt
import parsley.Parsley.pure
object Parser{
    import parsley.combinator.{endBy1, sepBy1, sepEndBy, sepEndBy1}
    import parsley.expr.{precedence, Ops, InfixL, Prefix}
    import parsley.expr.chain
    import Lexing.lexer
    import Lexing._
    import implicits.implicitSymbol
    import AST._


    val BOOL_LIT = lexer.lexeme.symbol("true") #> true <|> 
                   lexer.lexeme.symbol("false") #> false
                                 
    val PAIR_LIT = lexer.lexeme.symbol("null") #> PairLiteralNull
    
    lazy val ARRAY_ELEM = ArrayElem(IDENT, endBy1("[" *> expr, "]"))

    val BASE_TYPE = lexer.lexeme.symbol("int") #> IntType <|>
                    lexer.lexeme.symbol("string") #> StringType <|>
                    lexer.lexeme.symbol("bool") #> BoolType <|> 
                    lexer.lexeme.symbol("char") #> CharType
    
    private lazy val atom: Parsley[Expr] = 
                    "(" *> expr <* ")" <|> IntLiteral(INTEGER) <|> CharLiteral(CHR_LIT) <|>
                    StrLiteral(STR_LIT) <|> BoolLiteral(BOOL_LIT) <|> Ident(IDENT) <|> PAIR_LIT

    val operators: Parsley[Expr] = precedence[Expr](
        atom)(
                      Ops(Prefix)(Length <# "len", Ord <# "ord", Chr <# "chr", Negate <# UNOP_MINUS, Not <# "!"),
                      Ops(InfixL)(Mul <# "*", Div <# "/", Mod <# "%"),
                      Ops(InfixL)(Add <# "+", Sub <# "-"),
                      Ops(InfixL)(Greater <# ">", GreaterEquals <# ">=", Less <# "<", LessEquals <# "<="),
                      Ops(InfixL)(Equal <# "==", NotEqual <# "!="),
                      Ops(InfixL)(And <# "&&"),
                      Ops(InfixL)(Or <# "||")
                   )
    
    val expr: Parsley[Expr] = atom <|> ARRAY_ELEM <|> operators

    val ARRAY_LITER = ArrayLiteral("[" *> (sepBy1(expr, ",") <* "]"))
    
    lazy val PAIR_ELEM_TYPE = lexer.lexeme.symbol("pair") #> Pair <|> chain.postfix(BASE_TYPE, ArrayType <# "[]")
    
    val PAIR_TYPE = PairType(lexer.lexeme.symbol("pair") *> "(" *> PAIR_ELEM_TYPE, "," *> PAIR_ELEM_TYPE <~ ")")
    
    private lazy val atom2: Parsley[Type] = BASE_TYPE <|> PAIR_TYPE

    val ARRAY_TYPE: Parsley[Type] = chain.postfix(atom2, ArrayType <# "[]")

    
    lazy val types: Parsley[Type] = ARRAY_TYPE <|> BASE_TYPE <|> PAIR_TYPE 

    val ARG_LIST = sepEndBy(expr, ",")

    lazy val PAIR_ELEM = Fst(lexer.lexeme.symbol("fst") *> lvalue) <|> Snd(lexer.lexeme.symbol("snd") *> lvalue)

    lazy val rvalue: Parsley[RValue] = expr <|> 
                                       ARRAY_LITER <|> 
                                       NewPair(lexer.lexeme.symbol("newpair") *> "(" *> expr, expr <~ ")") <|> 
                                       PAIR_ELEM <|> 
                                       Call(IDENT, "(" *> ARG_LIST <~ ")")

    lazy val lvalue: Parsley[LValue] = Ident(IDENT) <|>  ARRAY_ELEM <|> PAIR_ELEM

    val stat: Parsley[Stat] = (lexer.lexeme.symbol("skip") #> Skip) <|> 
                              (Declare(types, IDENT, lexer.lexeme.symbol("=") *> rvalue)) <|>
                              (Assign(lvalue, lexer.lexeme.symbol("=") *> rvalue)) <|>
                              (Read(lexer.lexeme.symbol("read") *> lvalue)) <|>
                              (Free(lexer.lexeme.symbol("free") *> expr)) <|>
                              (Return(lexer.lexeme.symbol("return") *> expr)) <|>
                              (Exit(lexer.lexeme.symbol("exit") *> expr)) <|>
                              (Print(lexer.lexeme.symbol("print") *> expr)) <|>
                              (Println(lexer.lexeme.symbol("println") *> expr)) <|>
                              (If(lexer.lexeme.symbol("if") *> expr,
                                  lexer.lexeme.symbol("then") *> stats, 
                                  lexer.lexeme.symbol("else") *> stats <~ lexer.lexeme.symbol("fi"))) <|>
                              (While(lexer.lexeme.symbol("while") *> expr,
                                  lexer.lexeme.symbol("do") *> stats <~ lexer.lexeme.symbol("done"))) <|>
                              (Begin(lexer.lexeme.symbol("stat") *> stats <~ lexer.lexeme.symbol("end"))) 
    
    private lazy val stats = sepEndBy(stat, ";")

    val param = Param(types, IDENT)

    val paramList = sepEndBy(param, ",")

    val func_ = attempt(Func_(types, IDENT <~ "("))

    val func = Func(func_, paramList <~ ")", lexer.lexeme.symbol("is") *> stats <~ lexer.lexeme.symbol("end"))
 
    val program_ = Program(lexer.lexeme.symbol("begin") *> sepEndBy(func, pure("")), sepEndBy1(stat, ";") <~ lexer.lexeme.symbol("end"))

    val program = fully(program_)
}


 