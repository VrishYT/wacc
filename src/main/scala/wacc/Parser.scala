package wacc
import parsley.Parsley
import parsley.Parsley.attempt
import parsley.Parsley.pure
object Parser{
    import parsley.combinator._
    import parsley.expr.{precedence, Ops, InfixL, Prefix}
    import parsley.expr.chain
    import parsley.errors.combinator._
    import Lexing.lexer
    import Lexing._
    import implicits.implicitSymbol
    import AST._


    val BOOL_LIT = (lexer.lexeme.symbol("true") #> true <|> 
                   lexer.lexeme.symbol("false") #> false).label("boolean (true or false)")
                                 
    val PAIR_LIT = ("null") #> PairLiteralNull
    
    lazy val ARRAY_ELEM = ArrayElem(IDENT, "[" *> sepBy(expr, "][") <* "]")

    val BASE_TYPE = lexer.lexeme.symbol("int").label("type \'int\'") #> IntType <|>
                    lexer.lexeme.symbol("string").label("type \'string\'") #> StringType <|>
                    lexer.lexeme.symbol("bool").label("type \'bool\'") #> BoolType <|> 
                    lexer.lexeme.symbol("char").label("type \'char\'") #> CharType
    
    private lazy val atom: Parsley[Expr] = 
                    "(" *> expr <* ")" <|> attempt(ARRAY_ELEM) <|> IntLiteral(INTEGER) <|> CharLiteral(CHR_LIT) <|>
                    StrLiteral(STR_LIT) <|> BoolLiteral(BOOL_LIT)  <|> Ident(IDENT) <|> PAIR_LIT


    def unary_op(x: Parsley[Unit]) = x.label("unary operator").explain("unary operators include len, ord, chr, ! and -")

    def arith_op(x: Parsley[Unit]) = x.label("arithmetic operator").explain("arithmetic operators include *, /, %, + and -")

    def comp_op(x: Parsley[Unit]) = x.label("comparison operator").explain("comparison operators include >, >=, <, <=, != and ==")

    def logic_op(x: Parsley[Unit]) = x.label("logical operator").explain("logical operators include && and ||")

    val operators: Parsley[Expr] = precedence[Expr](
        atom)(
                      Ops(Prefix)(Length <# unary_op(lexer.lexeme.symbol("len")), Ord <# unary_op(lexer.lexeme.symbol("ord")), Chr <# unary_op(lexer.lexeme.symbol("chr")), Negate <# unary_op(UNOP_MINUS), Not <# unary_op(lexer.lexeme.symbol("!"))),
                      Ops(InfixL)(Mul <# arith_op(lexer.lexeme.symbol("*")), Div <# arith_op(lexer.lexeme.symbol("/")), Mod <# arith_op(lexer.lexeme.symbol("%"))),
                      Ops(InfixL)(Add <# arith_op(lexer.lexeme.symbol("+")), Sub <# arith_op(lexer.lexeme.symbol("-"))),
                      Ops(InfixL)(Greater <# comp_op(lexer.lexeme.symbol(">")), GreaterEquals <# comp_op(lexer.lexeme.symbol(">=")),
                                  Less <# comp_op(lexer.lexeme.symbol("<")), LessEquals <# comp_op(lexer.lexeme.symbol("<="))),
                      Ops(InfixL)(Equal <# comp_op(lexer.lexeme.symbol("==")), NotEqual <# comp_op(lexer.lexeme.symbol("!="))),
                      Ops(InfixL)(And <# logic_op(lexer.lexeme.symbol("&&"))),
                      Ops(InfixL)(Or <# logic_op(lexer.lexeme.symbol("||")))
                   )

    val expr: Parsley[Expr] = operators <|> atom  

    val ARRAY_LITER = ArrayLiteral("[" *> (sepBy(expr, ",") <* "]")).label("array literal")
    
    lazy val PAIR_ELEM_TYPE = ("pair") #> Pair <|> chain.postfix(BASE_TYPE, ArrayType <# "[]")
    
    val PAIR_TYPE = PairType(lexer.lexeme.symbol("pair") *> "(" *> PAIR_ELEM_TYPE, "," *> PAIR_ELEM_TYPE <~ ")").label("type \'pair\'") // explain
    
    private lazy val atom2: Parsley[Type] = BASE_TYPE <|> PAIR_TYPE

    val ARRAY_TYPE: Parsley[Type] = chain.postfix(atom2, ArrayType <# "[]").label("array type") // explain

    
    lazy val types: Parsley[Type] = ARRAY_TYPE <|> BASE_TYPE <|> PAIR_TYPE 

    val ARG_LIST = sepEndBy(expr, ",")

    lazy val PAIR_ELEM = Fst(("fst").label("pair operator") *> lvalue) <|> Snd(("snd").label("pair operator") *> lvalue)

    lazy val rvalue: Parsley[RValue] = expr <|> 
                                       ARRAY_LITER <|> 
                                       NewPair(lexer.lexeme.symbol("newpair") *> "(" *> expr <~ ",", expr <~ ")") <|> // explain
                                       PAIR_ELEM <|> 
                                       Call(lexer.lexeme.symbol("call") *> IDENT, "(" *> ARG_LIST <~ ")") // explain

    lazy val lvalue: Parsley[LValue] = attempt(ARRAY_ELEM) <|> PAIR_ELEM <|> Ident(IDENT) // remove attempt?

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
                              (Begin(lexer.lexeme.symbol("begin") *> stats <~ lexer.lexeme.symbol("end"))) // explain where needed for each
    
    private lazy val stats = sepBy1(stat, ";")

    val param = Param(types, IDENT).label("parameter")

    val paramList = sepBy(param, ",")

    val func_ = attempt(Func_(types, IDENT <~ "("))

    def validReturn(stats: List[Stat]): Boolean = stats.last match {
        case _: Return | _: Exit => true
        case _ => {
            var valid = false 
            stats.foreach(stat => stat match {
                case If(_, x, y) => {
                    valid |= validReturn(x) && validReturn(y)
                }
                case Begin(xs) => {
                    valid |= validReturn(xs)
                }
                case _ => 
            })
            valid
        }
    }

    val func = Func(func_, paramList <~ ")", lexer.lexeme.symbol("is") *> stats <* lexer.lexeme.symbol("end"))
 
    val program_ = Program(lexer.lexeme.symbol("begin") *> sepEndBy(func.filterOut {
        case func if !validReturn(func.stats) => "function does not have a return/exit"
    }, pure("")), stats <* lexer.lexeme.symbol("end"))

    val program = fully(program_)
}

/*
turn func_ into a zipped thing

take out all lexer.lexeme.symbol calls

use a verified fail on function calls as an OR after attempting to parse type/ident/(

use factory bridges on array elems to make sure they identify no brackets as identifiers, so we remove the attempts

move the valid return stuff into the Func node, removing all stuff from the AST

 */


 