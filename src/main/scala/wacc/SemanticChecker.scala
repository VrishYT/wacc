package wacc

import AST._

object SemanticChecker {

    import scala.collection.immutable.Map
    import scala.collection.mutable.{Map => MapM}

    def typecheck(program: Program): Unit = {
        val statements = program.stats
        val functions = program.fs

        val vars = functions.map(func => (func.id -> func.t)).toMap
        functions.foreach(func => checkFunction(func, vars))

    }

    def checkFunction(func: Func, vars: Map[String, Type]): Unit = {
        val childVars = func.args.map(param => (param.id -> param.t)).toMap
        checkStatements(func.stats, vars ++ childVars)
    }

    def checkStatements(statements: List[Stat], vars: Map[String, Type]): Unit = {

        val childVars = MapM[String, Type]()

        def checkExpressionType(x: Expr, t: Seq[Type]): Unit = {
            val returnType = getExpressionReturnType(x)
            returnType match {
                case Some(x) => if (!(t contains x)) ???
                case None => ???
            }
        }
        
        def getExpressionReturnType(x: Expr): Option[Type] = x match {
            case _: IntLiteral => Some(IntType)
            case _: CharLiteral => Some(CharType)
            case _: StrLiteral => Some(StringType)
            case _: BoolLiteral => Some(BoolType)
            case Ident(id) => {
                val varType = childVars.get(id)
                varType match {
                    case None => vars.get(id)
                    case x => x
                }
            }
            case ArrayElem(_, exp :: _) => getExpressionReturnType(exp)
            case UnaryOpExpr(op, exp) => {
                Some(op match {
                    case Not => BoolType
                    case Negate => IntType
                    case Length => IntType
                    case Ord => IntType
                    case Chr => CharType
                })
            }
            case BinaryOpExpr(op, exp1, exp2) => {
                Some(op match {
                    case Mul => IntType
                    case Div => IntType
                    case Mod => IntType
                    case Add => IntType
                    case Sub => IntType
                    case Greater => BoolType
                    case GreaterEquals => BoolType
                    case Less => BoolType
                    case LessEquals => BoolType
                    case Equal => BoolType
                    case NotEqual => BoolType
                    case And => BoolType
                    case Or => BoolType
                })
            }
        }
        
        statements.foreach(statement => {
            statement match {
                case Declare(t, id, rhs) => ???
                case Assign(x, y) => ???
                case Read(x) => ??? 
                case Free(x) => ???
                case Return(x) => ??? 
                case Exit(x) => checkExpressionType(x, Seq(IntType))
                case Print(x, end) => checkExpressionType(x, Seq(StringType))
                case If(p, xs, ys) => {
                    checkExpressionType(p, Seq(BoolType))
                    val newChildVars = createChildVars(vars, childVars)
                    checkStatements(xs, newChildVars)
                    checkStatements(ys, newChildVars)
                }
                case While(p, xs) => {
                    checkExpressionType(p, Seq(BoolType))
                    checkStatements(xs, createChildVars(vars, childVars))
                }
                case Begin(xs) => checkStatements(xs, createChildVars(vars, childVars))
            }
        })
    }

    def createChildVars(parent: Map[String, Type], child: MapM[String, Type]): Map[String, Type] = child.toMap ++ parent
    
}