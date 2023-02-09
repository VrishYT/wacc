package wacc
import parsley.Parsley
import parsley.Parsley.{attempt, pure, lookAhead}

object Parser {

    import parsley.combinator._
    import parsley.expr.{precedence, Ops, InfixL, Prefix}
    import parsley.expr.chain
    import parsley.debug._
    import parsley.errors.combinator._
    import parsley.errors.patterns._
    import parsley.position._   
    import Lexing.lexer
    import Lexing._
    import implicits.implicitSymbol
    import AST._

    def unary_op(x: Parsley[Unit]) = x.label("unary operator").explain("unary operators include len, ord, chr, ! and -")

    def arith_op(x: Parsley[Unit]) = x.label("arithmetic operator").explain("arithmetic operators include *, /, %, + and -")

    def comp_op(x: Parsley[Unit]) = x.label("comparison operator").explain("comparison operators include >, >=, <, <=, != and ==")

    def logic_op(x: Parsley[Unit]) = x.label("logical operator").explain("logical operators include && and ||")

    def base_type_desc(x: Parsley[Unit]) = x.label("base type").explain("base types include int, string, char and bool")

    def pair_type_desc(x: Parsley[Unit]) = x.label("pair type").explain("")

    def array_type_desc(x: Parsley[Unit]) = x.label("[] (array type)").explain("")

    def pair_op(x: Parsley[Unit]) = x.label("pair operator").explain("pair operators include \'fst\' and \'snd\'")

    val BOOL_LIT = ("true" #> true <|> 
                   "false" #> false).label("boolean literal (true or false)")
                                 
    val PAIR_LIT = (pos <**> ("null") #> PairLiteralNull).label("pair null type")

    val BASE_TYPE = base_type_desc("int") #> IntType <|>
                    base_type_desc("string") #> StringType <|>
                    base_type_desc("bool") #> BoolType <|> 
                    base_type_desc("char") #> CharType

    val IDENT_OR_ARRAY_ELEM = IdentOrArrayElem(IDENT.label("identifier"), option("[".label("index (like \'xs[idx]\')") *> sepBy(expr, "][") <* "]")) // *> lookAhead("(").explain("function calls dododo")
    
    private lazy val atom: Parsley[Expr] = 
                    "(".label("open parenthesis") *> expr <* ")" <|> IDENT_OR_ARRAY_ELEM <|> IntLiteral(INTEGER) <|> CharLiteral(CHR_LIT) <|>
                    StrLiteral(STR_LIT) <|> BoolLiteral(BOOL_LIT) <|> PAIR_LIT


    val operators: Parsley[Expr] = precedence[Expr](
        atom)(
                      Ops(Prefix)(Length <# unary_op("len"), Ord <# unary_op("ord"), Chr <# unary_op("chr"), Negate <# unary_op(UNOP_MINUS), Not <# unary_op("!")),
                      Ops(InfixL)(Mul <# arith_op("*"), Div <# arith_op("/"), Mod <# arith_op("%")),
                      Ops(InfixL)(Add <# arith_op("+"), Sub <# arith_op("-")),
                      Ops(InfixL)(Greater <# comp_op(">"), GreaterEquals <# comp_op(">="),
                                  Less <# comp_op("<"), LessEquals <# comp_op("<=")),
                      Ops(InfixL)(Equal <# comp_op("=="), NotEqual <# comp_op("!=")),
                      Ops(InfixL)(And <# logic_op("&&")),
                      Ops(InfixL)(Or <# logic_op("||"))
                   )

    val expr: Parsley[Expr] = operators <|> atom  

    val ARRAY_LITER = ArrayLiteral("[" *> (sepBy(expr, ",") <* "]")).label("array literal")
    
    lazy val PAIR_ELEM_TYPE = ("pair" #> Pair <|> chain.postfix(BASE_TYPE, ArrayType <# "[]")) // separate these
    
    val PAIR_TYPE = PairType(pair_type_desc("pair" *> "(".label("open parenthesis")) *> PAIR_ELEM_TYPE, "," *> PAIR_ELEM_TYPE <~ ")") // explain
    
    private lazy val atom2: Parsley[Type] = BASE_TYPE <|> PAIR_TYPE

    val ARRAY_TYPE: Parsley[Type] = chain.postfix(atom2, ArrayType <# array_type_desc("[]")) // explain

    
    lazy val types: Parsley[Type] = ARRAY_TYPE <|> BASE_TYPE <|> PAIR_TYPE 

    val ARG_LIST = sepEndBy(expr, ",")

    lazy val PAIR_ELEM = Fst(pair_op("fst") *> lvalue) <|> Snd(pair_op("snd") *> lvalue)

    val _invalid_call = amend(attempt(IDENT <~ "(") *> unexpected("opening parenthesis")).explain("function calls may not appear in expressions and must use `call`")

    lazy val rvalue: Parsley[RValue] = _invalid_call <|> Call("call".label("function call") *> IDENT, "(" *> ARG_LIST <~ ")") <|> // explain
                                       expr <|> 
                                       ARRAY_LITER <|> 
                                       NewPair("newpair" *> "(" *> expr <~ ",", expr <~ ")") <|> // explain
                                       PAIR_ELEM
                                       

    lazy val lvalue: Parsley[LValue] = IDENT_OR_ARRAY_ELEM <|> PAIR_ELEM

    val _invalid_declaration = amend(attempt((types *> IDENT <~ "(").hide) *> unexpected("function declaration")).explain("all functions must be declared at the top of main block") 

    val stat: Parsley[Stat] = _invalid_declaration <|> ("skip" #> Skip) <|>
                              (Declare(types, IDENT, "=" *> rvalue)) <|>
                              (Assign(lvalue, "=".label("assignment") *> rvalue)) <|>
                              (Read("read" *> lvalue)) <|>
                              (Free("free" *> expr)) <|>
                              (Return("return" *> expr)) <|>
                              (Exit("exit" *> expr)) <|>
                              (Print("print" *> expr)) <|>
                              (Println("println" *> expr)) <|>
                              (If("if" *> expr,
                                  "then" *> stats, 
                                  "else" *> stats <~ "fi")).label("if statement") <|>
                              (While("while" *> expr,
                                  "do" *> stats <~ ("done".explain("unclosed while loop")))).label("while statement") <|>
                              (Begin("begin" *> stats <~ "end")) // explain where needed for each

    // val _done_check =  // notFollowedBy("done").verifiedFail("unclosed while loop")

    private lazy val stats = sepBy1(stat, ";")

    val param = Param(types, IDENT)

    val paramList = sepBy(param, ",")

    val _invalid_function = amend((attempt(IDENT <~ "(").hide).verifiedFail("function declaration missing type")) // move into separate file (or object if it is the only one)

    val func = _invalid_function <|> Func(attempt(types <~> IDENT <~ "(".label("opening parenthesis")).label("function declaration"), paramList <~ ")", "is" *> stats <* "end") 

    val program_ = Program("begin" *> sepEndBy(func.guardAgainst {
        case func if !func.validReturn => Seq("function is missing a return/exit on all exit paths")
    }, pure("")), stats <* "end")

    val program = fully(program_)
}


 