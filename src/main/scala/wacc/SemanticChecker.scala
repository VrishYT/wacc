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

    def getTypeFromVars(id: String, parent: Map[String, Type], child: MapM[String, Type]): Option[Type] = {
        child.get(id) match {
            case None => parent.get(id)
            case x => x
        }
    }

    def checkStatements(statements: List[Stat], vars: Map[String, Type]): Unit = {

        val childVars = MapM[String, Type]()

        def getLValType(lval: LValue): Option[Type] = ???

        def checkReturnType(rval: RValue, t: Seq[Type]): Unit = {
            
            def getRValueReturnType(rval: RValue): Option[Type] = rval match {
                case ArrayLiteral(xs) => ???
                case NewPair(fst, snd) => ???
                case Call(id, args) => ???
                case x: Expr => x match {
                    case _: IntLiteral => Some(IntType)
                    case _: CharLiteral => Some(CharType)
                    case _: StrLiteral => Some(StringType)
                    case _: BoolLiteral => Some(BoolType)
                    case Ident(id) => getTypeFromVars(id, vars, childVars)
                    case ArrayElem(_, exp :: _) => getRValueReturnType(exp)
                    case UnaryOpExpr(op, exp) => {
                        val returnType = op match {
                            case Not => BoolType
                            case Negate => IntType
                            case Length => IntType
                            case Ord => IntType
                            case Chr => CharType
                        }
                        checkReturnType(exp, Seq(returnType))
                        Some(returnType)
                    }
                    case BinaryOpExpr(op, exp1, exp2) => {
                        val returnType = op match {
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
                        }
                        checkReturnType(exp1, Seq(returnType))
                        checkReturnType(exp2, Seq(returnType))
                        Some(returnType)
                    }
                }
            }

            val returnType = getRValueReturnType(rval)
            returnType match {
                case Some(x) => if (!(t contains x)) ???
                case None => ???
            }

        }
        
        statements.foreach(statement => {
            statement match {
                case Declare(t, id, rhs) => {
                    checkReturnType(rhs, Seq(t))
                    childVars(id) = t
                }
                case Assign(x, y) => {
                    val lType = getLValType(x)
                    lType match {
                        case Some(x) => checkReturnType(y, Seq(x))
                        case None => ???
                    }
                    
                }
                case Read(x) => ??? 
                case Free(x) => ??? // checkReturnType(x, Seq(PairType, ArrayType))
                case Return(x) => checkReturnType(x, Seq(IntType)) 
                case Exit(x) => checkReturnType(x, Seq(IntType))
                case If(p, xs, ys) => {
                    checkReturnType(p, Seq(BoolType))
                    val newChildVars = createChildVars(vars, childVars)
                    checkStatements(xs, newChildVars)
                    checkStatements(ys, newChildVars)
                }
                case While(p, xs) => {
                    checkReturnType(p, Seq(BoolType))
                    checkStatements(xs, createChildVars(vars, childVars))
                }
                case Begin(xs) => checkStatements(xs, createChildVars(vars, childVars))
            }
        })
    }

    def createChildVars(parent: Map[String, Type], child: MapM[String, Type]): Map[String, Type] = child.toMap ++ parent
    
}