package wacc

import AST._

object SemanticChecker {

    import scala.collection.immutable.Map
    import scala.collection.mutable.{Map => MapM}

    import ErrorLogger._

    def typecheck(program: Program): Unit = {
        val statements = program.stats
        val functions = program.fs

        val vars = functions.map(func => (func.fs.id -> func.fs.t)).toMap
        functions.foreach(func => checkFunction(func, vars))
        checkStatements(statements, vars)

    }

    def checkFunction(func: Func, vars: Map[String, Type]): Unit = {
        val childVars = func.args.map(param => (param.id -> param.t)).toMap
        checkStatements(func.stats, vars ++ childVars)
    }

    def getTypeFromVars(id: String, parent: Map[String, Type], child: MapM[String, Type]): Type = {
        child.get(id) match {
            case Some(x) => x
            case None => parent.get(id) match {
                case Some(x) => x
                case None => ErrorLogger.log("Variable " + id + " not found")
            }
        }
    }

    def checkStatements(statements: List[Stat], vars: Map[String, Type]): Unit = {

        val childVars = MapM[String, Type]()

        def getLValType(lVal: LValue): Type = 
            lVal match {
                case Ident(id) => getTypeFromVars(id, vars, childVars)                               
                case ArrayElem(id, _) => getTypeFromVars(id, vars, childVars) match {
                    case ArrayType(t) => t
                    case _ => ErrorLogger.log("unable to access non-array var as an array") // TODO
                }
                case x: PairElem => x match {
                    case Fst(x) => getLValType(x)
                    case Snd(x) => getLValType(x)
                }
        }


        def checkReturnType(rval: RValue, t: Seq[Type]): Unit = {
            
            def getRValueReturnType(rval: RValue): Type = rval match {
                case Fst(lval) => getLValType(lval) 
                case Snd(lval) => getLValType(lval) 
                case ArrayLiteral(xs) => {
                    if (xs.length > 1) {
                        new ArrayType(getRValueReturnType(xs.head))
                    } else {
                        new ArrayType(AnyType)
                    }
                }
                case NewPair(fst, snd) => new PairType(
                    getRValueReturnType(fst) match {
                        case x: PairElemType => x
                        case _ => ErrorLogger.log("not a pair elem type") 
                    }, 
                    getRValueReturnType(snd) match {
                        case x: PairElemType => x
                        case _ => ErrorLogger.log("not a pair elem type") 
                    }
                )
                case Call(id, args) => {
                    // TODO: verify args type
                    getTypeFromVars(id, vars, childVars)
                }
                case x: Expr => x match {
                    case _: IntLiteral => IntType
                    case _: CharLiteral => CharType
                    case _: StrLiteral => StringType
                    case _: BoolLiteral => BoolType
                    case Ident(id) => getTypeFromVars(id, vars, childVars)
                    case ArrayElem(_, exp :: _) => getRValueReturnType(exp)
                    case ArrayElem(_, Nil) => ErrorLogger.log("cannot have array elem with no expr")
                    case UnaryOpExpr(op, exp) => {
                        val types = op match {
                            case Not => (BoolType, BoolType)
                            case Negate => (IntType, IntType)
                            case Length => (IntType, IntType)
                            case Ord => (CharType, IntType)
                            case Chr => (IntType, CharType)
                        }
                        checkReturnType(exp, Seq(types._1))
                        types._2
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
                        returnType
                    }
                    case _ => ErrorLogger.log("")
                }
            }

            val returnType = getRValueReturnType(rval)
            
            if (!(t contains returnType)) {
                ErrorLogger.log(returnType.toString + " should be of type: " + t.mkString(","))
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
                    checkReturnType(y, Seq(lType))                    
                }
                case Read(x) => 
                case Free(x) => x match {
                    case x: ArrayType => 
                    case x: PairType => 
                    case x => ErrorLogger.log(x.toString + " should be of type: Array, Pair")
                }
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
                case default => 
            }
        })
    }

    def createChildVars(parent: Map[String, Type], child: MapM[String, Type]): Map[String, Type] = child.toMap ++ parent
    
}