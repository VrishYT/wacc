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

        def getLValType(lVal: LValue): Type = lVal match {
            case Ident(id) => getTypeFromVars(id, vars, childVars)                               
            case ArrayElem(id, _) => getTypeFromVars(id, vars, childVars) match {
                case ArrayType(t) => t
                case _ => ErrorLogger.log("unable to access non-array var as an array") // TODO
            }
            case x: PairElem => x match {
                case Fst(x) => getLValType(x) match {
                    case p: PairType => p.fst 
                    case _ => ErrorLogger.log("cannot evaluate fst on non-pair type")
                } 
                case Snd(x) => getLValType(x) match {
                    case p: PairType => p.snd 
                    case _ => ErrorLogger.log("cannot evaluate snd on non-pair type")
                } 
            }
        }
            
        def getRValType(rval: RValue): Type = rval match {
            case Fst(lval) => getLValType(lval) match {
                case p: PairType => p.fst 
                case _ => ErrorLogger.log("cannot evaluate fst on non-pair type")
            } 
            case Snd(lval) => getLValType(lval) match {
                case p: PairType => p.snd 
                case _ => ErrorLogger.log("cannot evaluate snd on non-pair type")
            } 
            case ArrayLiteral(xs) => {
                if (xs.length > 1) {
                    // TODO: verify type of all elems
                    new ArrayType(getRValType(xs.head))
                } else {
                    new ArrayType(AnyType)
                }
            }
            case NewPair(fst, snd) => new PairType(
                getRValType(fst) match {
                    case x: PairElemType => x
                    case _ => ErrorLogger.log("not a pair elem type") 
                    }, 
                    getRValType(snd) match {
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
                case PairLiteralNull => PairType(null, null)
                case Ident(id) => getTypeFromVars(id, vars, childVars)
                case ArrayElem(_, exp :: _) => getRValType(exp)
                case ArrayElem(_, Nil) => ErrorLogger.log("cannot have array elem with no expr")
                case UnaryOpExpr(op, exp) => {
                    val types = op match {
                        case Not => (BoolType, BoolType)
                        case Negate => (IntType, IntType)
                        case Length => (IntType, IntType)
                        case Ord => (CharType, IntType)
                        case Chr => (IntType, CharType)
                    }
                    if (getRValType(exp) != types._1) ErrorLogger.log("invalid type for unary op param")
                    types._2
                }
                case BinaryOpExpr(op, exp1, exp2) => {
                    def checkType(exp: Expr, ReturnType: Type): Unit = {
                        if (ReturnType == AnyType) return
                        getRValType(exp) match {
                            case ReturnType => 
                            case _ => ErrorLogger.log("invalid binary op type")
                        }
                    }
                    val returnType = op match {
                        case Mul | Div | Mod | Add | Sub => (IntType, IntType, IntType)
                        case Equal | NotEqual => (AnyType, AnyType, BoolType)
                        case And | Or => (BoolType, BoolType, BoolType)
                        case Greater | GreaterEquals | Less | LessEquals => {
                            val t1 = getRValType(exp1)
                            val t2 = getRValType(exp2)

                            def validType(t: Type) = t == CharType || t == IntType 

                            if (validType(t1) && validType(t2) && t1 == t2) return BoolType
                            else ErrorLogger.log("invalid equality type for binary op")
                        }
                    }
                    checkType(exp1, returnType._1)
                    checkType(exp1, returnType._2)
                    returnType._3
                }
                case _ => ErrorLogger.log("")
            }
        }
        
        statements.foreach(statement => {
            statement match {
                case Declare(t, id, rhs) => {
                    val rType = getRValType(rhs)
                    if (rType != t) ErrorLogger.log("invalid type for declare. expected: " + t  + ", actual: " + rType)
                    childVars(id) = t
                }
                case Assign(x, y) => {
                    val lType = getLValType(x)
                    val rType = getRValType(y)
                    if (lType != rType) ErrorLogger.log("invalid type for assign. expected : " + lType + ", actual : " + rType)              
                }
                case Read(x) => {
                    val ltype = getLValType(x)
                    if (ltype != IntType && ltype != CharType) ErrorLogger.log("invalid type for read. expected: <IntType, CharType>, actual: " + ltype)  
                }
                case Free(x) => {
                    val rType = getRValType(x)
                    rType match {
                        case x: ArrayType => 
                        case x: PairType =>
                        case x => ErrorLogger.log("invalid type for free. expected: <Array, Pair>, actual: " + rType)
                    }
                }
                case Return(x) => if (getRValType(x) != IntType) ErrorLogger.log("invalid type for assign")
                case Exit(x) => {
                    if (getRValType(x) != IntType) ErrorLogger.log("invalid type for exit")              
                }
                case If(p, xs, ys) => {
                    if (getRValType(p) != BoolType) ErrorLogger.log("invalid type for if cond") 
                    val newChildVars = createChildVars(vars, childVars)
                    checkStatements(xs, newChildVars)
                    checkStatements(ys, newChildVars)
                }
                case While(p, xs) => {
                    if (getRValType(p) != BoolType) ErrorLogger.log("invalid type for while cond") 
                    checkStatements(xs, createChildVars(vars, childVars))
                }
                case Begin(xs) => checkStatements(xs, createChildVars(vars, childVars))
                case default => 
            }
        })
    }

    def createChildVars(parent: Map[String, Type], child: MapM[String, Type]): Map[String, Type] = child.toMap ++ parent
    
}