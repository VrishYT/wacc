package wacc
import parsley.Parsley
import parsley.Parsley.attempt
import parsley.Parsley.pure
object Parser{
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
                   "false" #> false).label("boolean (true or false)")
                                 
    val PAIR_LIT = ("null") #> PairLiteralNull

    val BASE_TYPE = "int".label("type \'int\'") #> IntType <|>
                    "string".label("type \'string\'") #> StringType <|>
                    "bool".label("type \'bool\'") #> BoolType <|> 
                    "char".label("type \'char\'") #> CharType
    
    private lazy val atom: Parsley[Expr] = 
                    "(" *> expr <* ")" <|> IdentOrArrayElem(IDENT, option("[" *> sepBy(expr, "][") <* "]")) <|> IntLiteral(INTEGER) <|> CharLiteral(CHR_LIT) <|>
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
    
    lazy val PAIR_ELEM_TYPE = ("pair") #> Pair <|> chain.postfix(BASE_TYPE, ArrayType <# "[]") // separate these
    
    val PAIR_TYPE = PairType("pair" *> "(" *> PAIR_ELEM_TYPE, "," *> PAIR_ELEM_TYPE <~ ")").label("type \'pair\'") // explain
    
    private lazy val atom2: Parsley[Type] = BASE_TYPE <|> PAIR_TYPE

    val ARRAY_TYPE: Parsley[Type] = chain.postfix(atom2, ArrayType <# "[]").label("array type") // explain

    
    lazy val types: Parsley[Type] = ARRAY_TYPE <|> BASE_TYPE <|> PAIR_TYPE 

    val ARG_LIST = sepEndBy(expr, ",")

    lazy val PAIR_ELEM = Fst(("fst").label("pair operator") *> lvalue) <|> Snd(("snd").label("pair operator") *> lvalue)

    lazy val rvalue: Parsley[RValue] = expr <|> 
                                       ARRAY_LITER <|> 
                                       NewPair("newpair" *> "(" *> expr <~ ",", expr <~ ")") <|> // explain
                                       PAIR_ELEM <|> 
                                       Call("call" *> IDENT, "(" *> ARG_LIST <~ ")") // explain

    lazy val lvalue: Parsley[LValue] = IdentOrArrayElem(IDENT, option("[" *> sepBy(expr, "][") <* "]")) <|> PAIR_ELEM // remove attempt?

    val stat: Parsley[Stat] = ("skip" #> Skip) <|> 
                              (Declare(types, IDENT, "=" *> rvalue)) <|>
                              (Assign(lvalue, "=" *> rvalue)) <|>
                              (Read("read" *> lvalue)) <|>
                              (Free("free" *> expr)) <|>
                              (Return("return" *> expr)) <|>
                              (Exit("exit" *> expr)) <|>
                              (Print("print" *> expr)) <|>
                              (Println("println" *> expr)) <|>
                              (If("if" *> expr,
                                  "then" *> stats, 
                                  "else" *> stats <~ "fi")) <|>
                              (While("while" *> expr,
                                  "do" *> stats <~ "done")) <|>
                              (Begin("begin" *> stats <~ "end")) // explain where needed for each
    
    private lazy val stats = sepBy1(stat, ";")

    val param = Param(types, IDENT).label("parameter")

    val paramList = sepBy(param, ",")

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

    val invalid_function = IDENT <~ "("

    val func = Func(attempt(types <~> IDENT <~ "("), paramList <~ ")", "is" *> stats <* "end") <|>
                invalid_function.verifiedFail("missing return type of function")
 
    val program_ = Program("begin" *> sepEndBy(func.filterOut {
        case func if !validReturn(func.stats) => "function does not have a return/exit"
    }, pure("")), stats <* "end")

    val program = fully(program_)
}

/*
move the valid return stuff into the Func node, removing all stuff from the AST

 */


 