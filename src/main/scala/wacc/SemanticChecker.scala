package wacc

import AST._

object SemanticChecker {

    import scala.collection.immutable.Map
    import scala.collection.mutable.{Map => MapM}

    def typecheck(program: Program): Unit = {
        val statements = program.stats
        val functions = program.fs

        functions.foreach(func => {
            checkFunction(func)        
        })

    }

    def checkFunction(func: Func): Unit = {
        val vars = func.args.map(param => (param.id -> param.t)).toMap
        checkStatements(func.stats, vars)
    }

    def checkStatements(statements: List[Stat], vars: Map[String, Type]): Unit = {

        val childVars = MapM[String, Type]()

        def checkExpressionType(x: Expr, t: Seq[Type]): Unit = {
            x match {
                case _: IntLiteral => t contains IntType
                case _: CharLiteral => t contains CharType
                case _: StrLiteral => t contains StringType
                case _: BoolLiteral => t contains BoolType
                case Ident(id) => {
                    val varType = childVars.get(id)
                    varType match {
                        case Some(x) => t contains x
                        case None => {
                            val varType = vars.get(id)
                            varType match {
                                case Some(x) => t == x
                                case None => ??? // error
                            }
                        }
                    }
                }
                case ArrayElem(_, exp :: _) => checkExpressionType(exp, t)
                case UnaryOpExpr(op, exp) => {
                    val opType = op match {
                        case Not => BoolType
                        case Negate => IntType
                        case Length => IntType
                        case Ord => IntType
                        case Chr => CharType
                    }

                    if (!(t contains opType)) {
                        false
                    } else {
                        checkExpressionType(exp, t)
                    }

                }
                case BinaryOpExpr(op, exp1, exp2) => ???
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
                case Print(x, end) => ??? 
                case If(p, x, y) => ??? 
                case While(p, x) => ??? 
                case Begin(xs) => checkStatements(xs, createChildVars(vars, childVars))
            }
        })
    }

    def checkExpressionType(x: Expr, t: Type): Boolean = {
        x match {
            case IntLiteral(_) => ???
            case CharLiteral(_) => ???
            case StrLiteral(_) => ???
            case BoolLiteral(_) => ???
            case ArrayElem(_, _) => ???
            case UnaryOpExpr(_, _) => ???
            case BinaryOpExpr(_, _, _) => ???
        }
    }

    def createChildVars(parent: Map[String, Type], child: MapM[String, Type]): Map[String, Type] = child.toMap ++ parent
    
}