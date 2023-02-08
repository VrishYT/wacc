package wacc
import parsley.Parsley
import parsley.Parsley.attempt
import parsley.Parsley.pure

object Parser {

    import parsley.combinator._
    import parsley.expr.{precedence, Ops, InfixL, Prefix}
    import parsley.expr.chain
    import parsley.errors.combinator._
    import parsley.errors.patterns._
    import Lexing.lexer
    import Lexing._
    import implicits.implicitSymbol
    import AST._

    val BOOL_LIT = ("true" #> true <|> 
                   "false" #> false).label("boolean literal (true or false)")
                                 
    val PAIR_LIT = (("null") #> PairLiteralNull).label("pair null type")

    def type_desc(x: Parsley[Unit]) = x.label("type").explain("types include int, string, char and bool")

    val BASE_TYPE = type_desc("int") #> IntType <|>
                    type_desc("string") #> StringType <|>
                    type_desc("bool") #> BoolType <|> 
                    type_desc("char") #> CharType
    
    private lazy val atom: Parsley[Expr] = 
                    "(".label("open parenthesis") *> expr <* ")" <|> IdentOrArrayElem(IDENT, option("[" *> sepBy(expr, "][") <* "]")) <|> IntLiteral(INTEGER) <|> CharLiteral(CHR_LIT) <|>
                    StrLiteral(STR_LIT) <|> BoolLiteral(BOOL_LIT) <|> PAIR_LIT


    def unary_op(x: Parsley[Unit]) = x.label("unary operator").explain("unary operators include len, ord, chr, ! and -")

    def arith_op(x: Parsley[Unit]) = x.label("arithmetic operator").explain("arithmetic operators include *, /, %, + and -")

    def comp_op(x: Parsley[Unit]) = x.label("comparison operator").explain("comparison operators include >, >=, <, <=, != and ==")

    def logic_op(x: Parsley[Unit]) = x.label("logical operator").explain("logical operators include && and ||")

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
    
    lazy val PAIR_ELEM_TYPE = (("pair").hide #> Pair <|> chain.postfix(BASE_TYPE, ArrayType <# "[]")) // separate these
    
    val PAIR_TYPE = PairType("pair".hide *> "(" *> PAIR_ELEM_TYPE, "," *> PAIR_ELEM_TYPE <~ ")") // explain
    
    private lazy val atom2: Parsley[Type] = BASE_TYPE <|> PAIR_TYPE.label("type \'pair\'")

    val ARRAY_TYPE: Parsley[Type] = chain.postfix(atom2, ArrayType <# "[]".label("array type")) // explain

    
    lazy val types: Parsley[Type] = ARRAY_TYPE <|> BASE_TYPE <|> PAIR_TYPE 

    val ARG_LIST = sepEndBy(expr, ",")

    lazy val PAIR_ELEM = Fst(("fst").label("pair operator").explain("pair operators include fst and snd") *> lvalue) <|> Snd(("snd").label("pair operator").explain("pair operators include fst and snd") *> lvalue)

    lazy val rvalue: Parsley[RValue] = expr <|> 
                                       ARRAY_LITER <|> 
                                       NewPair("newpair" *> "(" *> expr <~ ",", expr <~ ")") <|> // explain
                                       PAIR_ELEM <|> 
                                       Call("call".label("function call") *> IDENT, "(" *> ARG_LIST <~ ")") // explain

    lazy val lvalue: Parsley[LValue] = IdentOrArrayElem(IDENT.label("identifier"), option("[".label("index (like \'xs[idx]\')") *> sepBy(expr, "][") <* "]")) <|> PAIR_ELEM

    val stat: Parsley[Stat] = ("skip" #> Skip) <|> 
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
                                  "do" *> stats <~ "done")) <|>
                              (Begin("begin" *> stats <~ "end")) // explain where needed for each
    
    private lazy val stats = sepBy1(stat, ";")

    val param = Param(types, IDENT).label("parameter")

    val paramList = sepBy(param, ",")

    val invalid_function = IDENT <~ "(" // move into separate file (or object if it is the only one)

    val func = Func(attempt(types <~> IDENT <~ "(").label("function declaration"), paramList <~ ")", "is" *> stats <* "end") <|>
                invalid_function.verifiedFail("missing return type of function")
 
    // val program_ = Program("begin" *> sepEndBy(func, pure("")).guardAgainst {
    //     case fs if fs.nonEmpty && fs.map(fs.tail).exi
    // }, stats <* "end")

    val program_ = Program("begin" *> sepEndBy(func.guardAgainst {
        case func if !func.validReturn => Seq("function not return/exit")
    }, pure("")), stats <* "end")

    val program = fully(program_)
}

/*func.filterOut {
        case func if !validReturn(func.stats) => "function does not have a return/exit"
    },
*/
/*
move the valid return stuff into the Func node, removing all stuff from the AST

 */


 