package wacc
package front

import parsley.Parsley
import parsley.Parsley.{attempt, empty, notFollowedBy, pure}

object Parser {

  import Lexing._
  import implicits.implicitSymbol
  import parsley.combinator._
  import parsley.Parsley.lookAhead
  import parsley.errors.combinator._
  import parsley.expr._
  import parsley.position._
  import wacc.ast._

  /*functions that provide labels to specific types in wacc to improve error messages */
  def unary_op(x: Parsley[Unit]) = x.label("unary operator").explain(
    "unary operators include len, ord, chr, ! and -")

  def arith_op(x: Parsley[Unit]) = x.label("arithmetic operator").explain(
    "arithmetic operators include *, /, %, + and -")

  def comp_op(x: Parsley[Unit]) = x.label("comparison operator").explain(
    "comparison operators include >, >=, <, <=, != and ==")

  def logic_op(x: Parsley[Unit]) = x.label("logical operator").explain(
    "logical operators include && and ||")

  def base_type_desc(x: Parsley[Unit]) = x.label("base type").explain(
    "base types include int, string, char and bool")

  def pair_type_desc(x: Parsley[Unit]) = x.label("pair type")

  def array_type_desc(x: Parsley[Unit]) = x.label("[] (array type)")

  def pair_op(x: Parsley[Unit]) = x.label("pair operator").explain(
    "pair operators include \'fst\' and \'snd\'")

  /*parsing of basic types such as boolean values, pair literal null and primitive types*/
  val BOOL_LIT = ("true" #> true <|> "false" #> false).label(
    "boolean literal").explain("booleans can either be true or false")
  
  lazy val PRIVATE = ("private" #> true <|> "public" #> false) <|> lookAhead(IDENT <|> types) #> false 

  val PAIR_LIT = (pos <**> ("null") #> PairLiteralNull).label("pair null type")

  val BASE_TYPE = base_type_desc("int") #> IntType <|>
    base_type_desc("string") #> StringType <|>
    base_type_desc("bool") #> BoolType <|>
    base_type_desc("char") #> CharType 
  
  val annotation = "@" *> Annotation(IDENT)
  
  val annotationList = sepBy(annotation.guardAgainst {
    case annotation if !annotation.isValid => Seq(annotation.errorMsg) // TODO: improve error msg
  }, pure(""))


  /*labels method allows us to print the expected values when we throw an unexpected error*/
  private def labels(ls: String*) = choice(ls.map(empty.label(_)): _*)

  /*invalid call ensures that a function isn't called without the keyword call*/
  private val invalid_call = notFollowedBy("(").explain(
    "function calls may not appear in expressions and must use `call`") <|>
    unexpected("opening parenthesis") <|>
    labels("arithmetic operator", "logical operator",
      "comparison operator", "index (like \'xs[idx]\')")

  /*ident or class elem or array elem checks if the text is only an ident or a class elem (<ident> (.<ident>)*)
      it is an array elem (it is followed by a [) and creates the corresponding AST node*/
  val L_EXPR = LExpr(sepBy1("this" #> "this" <|> IDENT, ".").label("identifier/class elem"), invalid_call *> option(
    "[".label("index (like \'xs[idx]\')") *> sepBy(expr, "][") <* "]")).guardAgainst {
      case x if (!x.valid) => Seq("Cannot access the elements of an array within a class directly.")
    }

  /*base elements of any expression, as the expression type is recursive*/
  private lazy val atom: Parsley[Expr] = "(".label("open parenthesis") *> expr <* ")" <|>
    // attempt(CLASS_ELEM) <|>
    L_EXPR <|>
    IntLiteral(INTEGER.hide).label("integer literal").explain(
      "all numbers are signed 32-bit integers") <|>
    CharLiteral(CHR_LIT) <|>
    StrLiteral(STR_LIT) <|>
    BoolLiteral(BOOL_LIT) <|>
    PAIR_LIT 

  /*operators in expression are given a precedence from tightest binding to loosest*/
  val operators: Parsley[Expr] = precedence[Expr](atom)(
    Ops(Prefix)(Length <# unary_op("len"), Ord <# unary_op("ord"),
      Chr <# unary_op("chr"),
      Negate <# unary_op(UNOP_MINUS),
      Not <# unary_op("!")),
    Ops(InfixL)(Mul <# arith_op("*"), Div <# arith_op("/"),
      Mod <# arith_op("%")),
    Ops(InfixL)(Add <# arith_op("+"), Sub <# arith_op("-")),
    Ops(InfixL)(Greater <# comp_op(">"),
      GreaterEquals <# comp_op(">="),
      Less <# comp_op("<"),
      LessEquals <# comp_op("<=")),
    Ops(InfixL)(Equal <# comp_op("=="), NotEqual <# comp_op("!=")),
    Ops(InfixL)(And <# logic_op("&&")),
    Ops(InfixL)(Or <# logic_op("||")))

  val expr: Parsley[Expr] = operators <|> atom

  val ARRAY_LITER = ArrayLiteral("[" *> (sepBy(expr, ",") <* "]")).label("array literal")

  lazy val PAIR_ELEM_TYPE = ("pair" #> Pair <|> chain.postfix(BASE_TYPE, ArrayType <# "[]"))

  val PAIR_TYPE = PairType(pair_type_desc("pair" *> "(".label(
    "open parenthesis")) *> PAIR_ELEM_TYPE, "," *> PAIR_ELEM_TYPE <~ ")")

  /*as the array type is recursive we need to define the basic type of an array which
      can either be a primitive type or a pair i.e. you cannot have arrays of arrays infinitely*/
  private lazy val atom2: Parsley[Type] = BASE_TYPE <|> PAIR_TYPE

  val ARRAY_TYPE: Parsley[Type] = chain.postfix(atom2, ArrayType <# array_type_desc("[]"))

  val CLASS_TYPE: Parsley[Type] = ClassType(IDENT <~ lookAhead(IDENT))

  lazy val types: Parsley[Type] = ARRAY_TYPE <|> BASE_TYPE <|> PAIR_TYPE <|> CLASS_TYPE

  val ARG_LIST = sepEndBy(expr, ",")

  lazy val PAIR_ELEM = Fst(pair_op("fst") *> lvalue) <|> Snd(pair_op("snd") *> lvalue)

  /*defined parsing for r-values*/
  lazy val rvalue: Parsley[RValue] = Call("call".label("function call") *> sepBy("this" #> "this" <|> IDENT, "."), "(" *> ARG_LIST <~ ")") <|> //TODO change for a list of idents
    expr <|>
    ARRAY_LITER <|>
    NewPair("newpair" *> "(" *> expr <~ ",", expr <~ ")") <|>
    NewClass("new" *> IDENT, "{" *> sepBy(rvalue, ",") <~ "}") <|> // TODO: CHECK @PREESHA- braces instead of parentheses ???
    PAIR_ELEM 

  /*defined parsing for l-values*/
  lazy val lvalue: Parsley[LValue] = L_EXPR <|> PAIR_ELEM  

  /*created a parsing rule to avoid function declarations in the middle of a block*/
  val _invalid_declaration = amend(attempt((types *> IDENT <~ "(").hide) *> unexpected(
    "function declaration")).explain(
    "all functions must be declared at the top of main block")

  /*defined parsing for statements*/
  val stat: Parsley[Stat] = _invalid_declaration <|>
    ("skip" #> Skip) <|>
    (pos <**> "continue" #> Continue) <|>
    (pos <**> "break" #> Break) <|>
    attempt(AssignOrTypelessDeclare(annotationList, attempt(lvalue <~ "=".label("assignment")), rvalue)) <|>
    attempt(Declare(annotationList, types, IDENT, "=" *> rvalue)) <|>
    (Read("read" *> lvalue)) <|>
    (Free("free" *> expr)) <|>
    (Return("return" *> expr)) <|>
    (Exit("exit" *> expr)) <|>
    (Print("print" *> expr)) <|>
    (Println("println" *> expr)) <|>
    (If("if" *> expr, "then".explain(
      "all if statements must have an then statement") *> stats,
      "else".explain(
        "all if statements must have an else statement") *> stats <~ "fi".explain(
        "unclosed if statement"))).label("if statement") <|>
    (While("while" *> expr,
      "do" *> stats <~ ("done".explain(
        "unclosed while loop")))).label("while loop") <|>
    (Begin("begin" *> stats <~ "end"))

  /*parsing for a list of statements, that is used within the statement above*/
  private lazy val stats = sepBy1(stat, ";")

  /*rule to pick up on invalid pointer usage in parameters*/
  val _invalid_pointer = amend((attempt("*")) *> unexpected("pointer").explain(
    "WACC is not like C and does not use pointers"))

  val param = attempt(TypedParam(types, _invalid_pointer <|> IDENT)) <|> TypelessParam(_invalid_pointer <|> IDENT)

  val paramList = sepBy(param, ",")

  /*rule to pick on invalid function declarations with a missing type*/
  // val _invalid_function = amend((attempt(IDENT <~ "(").hide).verifiedFail(
  //   "function declaration missing type"))

  val func = attempt(TypedFunc(annotationList, PRIVATE, types <~> IDENT <~ "(".label(
    "opening parenthesis").label(
    "function declaration"), paramList <~ ")", "is" *> stats <* "end")) <|>
    TypelessFunc(annotationList, PRIVATE, IDENT <~ "(".label(
    "opening parenthesis").label(
    "function declaration"), paramList <~ ")", "is" *> stats <* "end")
  
  val funcList = sepEndBy(attempt(func).guardAgainst { case func if !func.validReturn => Seq("function is missing a return/exit on all exit paths")}, pure(""))

  /*rule to parse on classes */
  val field = attempt(Field(PRIVATE, types, IDENT <* notFollowedBy("(")))
  val fieldList = sepBy(field, ",")
  val class_ = Class(annotationList, attempt("class" *> IDENT <~ "{"), fieldList, funcList <~ "}")
  val classList = sepEndBy(attempt(class_), pure(""))

  /*rule to parse on programs, we check that at the end of the function body
    we have a return or an exit on all exit paths using the method valid return*/
  val program_ = Program(annotationList, "begin" *> classList, funcList, stats <* "end")

  val program = fully(program_)
}
