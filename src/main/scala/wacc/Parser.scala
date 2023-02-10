package wacc
import parsley.Parsley
import parsley.Parsley.{attempt, pure, lookAhead, notFollowedBy, empty}

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

    /*functions that provide labels to specific types in wacc to improve error messages */
    def unary_op(x: Parsley[Unit]) = x.label("unary operator").explain("unary operators include len, ord, chr, ! and -")

    def arith_op(x: Parsley[Unit]) = x.label("arithmetic operator").explain("arithmetic operators include *, /, %, + and -")

    def comp_op(x: Parsley[Unit]) = x.label("comparison operator").explain("comparison operators include >, >=, <, <=, != and ==")

    def logic_op(x: Parsley[Unit]) = x.label("logical operator").explain("logical operators include && and ||")

    def base_type_desc(x: Parsley[Unit]) = x.label("base type").explain("base types include int, string, char and bool")

    def pair_type_desc(x: Parsley[Unit]) = x.label("pair type").explain("")

    def array_type_desc(x: Parsley[Unit]) = x.label("[] (array type)").explain("")

    def pair_op(x: Parsley[Unit]) = x.label("pair operator").explain("pair operators include \'fst\' and \'snd\'")

    /*parsing of basic types such as boolean values, pair literal null and primitive types*/
    val BOOL_LIT = ("true" #> true <|> 
                   "false" #> false).label("boolean literal (true or false)")
                                 
    val PAIR_LIT = (pos <**> ("null") #> PairLiteralNull).label("pair null type")

    val BASE_TYPE = base_type_desc("int") #> IntType <|>
                    base_type_desc("string") #> StringType <|>
                    base_type_desc("bool") #> BoolType <|> 
                    base_type_desc("char") #> CharType
    
    
    /*labels method allows us to print the expected values when we throw an unexpected error*/
    private def labels(ls: String*) = choice(ls.map(empty.label(_)):_*)

    /*invalid call ensures that a function isn't called without the keyword call*/
    private val invalid_call = notFollowedBy("(").explain("function calls may not appear in expressions and must use `call`") <|> unexpected("opening parenthesis") <|>
                                               labels("arithmetic operator", "logical operator", "comparison operator", "index (like \'xs[idx]\')")

    
    /*ident or array elem checks if the text is only an ident or it is an array elem (it is followed by a [) and creates the corresponding AST node*/
    val IDENT_OR_ARRAY_ELEM = IdentOrArrayElem(IDENT.label("identifier"), invalid_call *> option("[".label("index (like \'xs[idx]\')") *> sepBy(expr, "][") <* "]" )) 
    
    /*base elements of any expression, as the expression type is recursive*/
    private lazy val atom: Parsley[Expr] = 
                    "(".label("open parenthesis") *> expr <* ")" <|> IDENT_OR_ARRAY_ELEM <|> IntLiteral(INTEGER.hide).label("integer literal")
                                    .explain("all numbers are signed 32-bit integers") <|> CharLiteral(CHR_LIT) <|>
                    StrLiteral(STR_LIT) <|> BoolLiteral(BOOL_LIT) <|> PAIR_LIT

    /*operators in expression are given a precedence from tightest binding to loosest*/
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
    
    lazy val PAIR_ELEM_TYPE = ("pair" #> Pair <|> chain.postfix(BASE_TYPE, ArrayType <# "[]")) 
    
    val PAIR_TYPE = PairType(pair_type_desc("pair" *> "(".label("open parenthesis")) *> PAIR_ELEM_TYPE, "," *> PAIR_ELEM_TYPE <~ ")") 
    
    /*as the array type is recursive we need to define the basic type of an array which can either be a primitive type or a pair
      i.e. you cannot have arrays of arrays infinitely*/
    private lazy val atom2: Parsley[Type] = BASE_TYPE <|> PAIR_TYPE

    val ARRAY_TYPE: Parsley[Type] = chain.postfix(atom2, ArrayType <# array_type_desc("[]")) // explain

    
    lazy val types: Parsley[Type] = ARRAY_TYPE <|> BASE_TYPE <|> PAIR_TYPE 

    val ARG_LIST = sepEndBy(expr, ",")

    lazy val PAIR_ELEM = Fst(pair_op("fst") *> lvalue) <|> Snd(pair_op("snd") *> lvalue)

    /*defined parsing for r-values*/
    lazy val rvalue: Parsley[RValue] = Call("call".label("function call") *> IDENT, "(" *> ARG_LIST <~ ")") <|> 
                                       expr <|> 
                                       ARRAY_LITER <|> 
                                       NewPair("newpair" *> "(" *> expr <~ ",", expr <~ ")") <|>
                                       PAIR_ELEM
                                       
    /*defined parsing for l-values*/
    lazy val lvalue: Parsley[LValue] = IDENT_OR_ARRAY_ELEM <|> PAIR_ELEM

    /*created a parsing rule to avoid function declarations in the middle of a block*/
    val _invalid_declaration = amend(attempt((types *> IDENT <~ "(").hide) *> unexpected("function declaration")).explain("all functions must be declared at the top of main block") 

    /*defined parsing for statements*/
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
                              (Begin("begin" *> stats <~ "end")) 

    /*parsing for a list of statements, that is used within the statement above*/
    private lazy val stats = sepBy1(stat, ";")

    /*rule to pick up on invalid pointer usage in parameters*/
    val _invalid_pointer = amend((attempt("*")) *> unexpected("pointer").explain("WACC is not like C and does not use pointers"))

    val param = Param(types, _invalid_pointer <|> IDENT)

    val paramList = sepBy(param, ",")

    /*rule to pick on invalid function declarations with a missing type*/
    val _invalid_function = amend((attempt(IDENT <~ "(").hide).verifiedFail("function declaration missing type")) 

    /*rule to parse on functions*/
    val func = _invalid_function <|> Func(attempt(types <~> IDENT <~ "(".label("opening parenthesis")).label("function declaration"), paramList <~ ")", "is" *> stats <* "end") 

    /*rule to parse on programs, we check that at the end of the function body we have a return or an exit on all exit paths using the method valid return*/
    val program_ = Program("begin" *> sepEndBy(func.guardAgainst {
        case func if !func.validReturn => Seq("function is missing a return/exit on all exit paths")
    }, pure("")), stats <* "end")

    val program = fully(program_)
}


 