package wacc

import AST._

object SemanticChecker {

    import scala.collection.immutable.Map
    import scala.collection.mutable.{Map => MapM}

    import ErrorLogger._

    def typecheck(program: Program): Unit = {
        val statements = program.stats
        val functions = program.fs

        /* Creates a Map for the global scope, containing function identifiers.
           Inserts a null identifier for null pairs. 
           Checks the semantics of each function.
           Checks semantics of each statement. */

        val vars = MapM[String, Type]()
        val funcArgs = MapM[String, List[Type]]()
        functions.foreach(func => {
            /* Check if the functions already exists */
            if (vars.keySet.exists(_ == func.fs._2)) ErrorLogger.err("Cannot redeclare function '" + func.fs._2 + "'")
            vars(func.fs._2) = func.fs._1
            funcArgs(func.fs._2) = func.args.map(arg => arg.t)
        })
        /* Produce maps of variables and functions to model the global scope */
        val varsImm = vars.toMap
        val funcArgsImm = funcArgs.toMap

        /* Check semantics of each function's statements */
        functions.foreach(func => checkFunction(func, varsImm, funcArgsImm))

        /* Check semantics of program's statements */
        checkStatements(statements, varsImm, funcArgsImm)
    }

    def declareVar(id: String, t: Type, vars: MapM[String, Type]): Unit = {
        if (vars.keySet.exists(_ == id)) ErrorLogger.err("Cannot redeclare variable '" + id + "'")
        vars(id) = t
    }

    /** Checks for invalid semantics within a specific function.
        @param func 
    */
    def checkFunction(func: Func, vars: Map[String, Type], funcArgs: Map[String, List[Type]]): Unit = {
        val childVars = MapM[String, Type]()
        func.args.foreach(param => declareVar(param.id, param.t, childVars))
        declareVar("\\func", func.fs._1, childVars) 

        /* Checks semantics of each statement in the function. */
        checkStatements(func.stats, createChildVars(vars, childVars), funcArgs)
    }

    /* Return the type of an identifier from the parent and child scope maps. */
    def getTypeFromVars(id: String, parent: Map[String, Type], child: MapM[String, Type] = MapM()): Type = {
        child.get(id) match {
            case Some(x) => x
            case None => parent.get(id) match {
                case Some(x) => x
                case None => ErrorLogger.err("Variable " + id + " not found")
            }
        }
    }

    /* Traverse a list of statements and error on semantic errors. */
    def checkStatements(statements: List[Stat], vars: Map[String, Type], funcArgs: Map[String, List[Type]]): Unit = {

        val childVars = MapM[String, Type]()

        /* Returns the type of the contents of a pairElem (fst(x) or snd(x), where x is passed in) */
        def getPairElemType(t: Type): Type = t match {
            case x: PairType => Pair
            case x: PairElemType => x
            case x => ErrorLogger.err("not a pair elem type. expected: <? extends PairElemType>, actual: " + x) 
        }

        /* Returns the type of a pairElem (fst or snd), errors if its not a PairElemType */
        def getLValPairElem(p: PairElem) = p match {
            case Fst(x) => getLValType(x) match {
                case PairType(fst, _) => getPairElemType(fst)
                case Pair | AnyType => AnyType
                case x => ErrorLogger.err("cannot evaluate fst on non-pair type: " + x)
            } 
            case Snd(x) => getLValType(x) match {
                case PairType(_, snd) => getPairElemType(snd)
                case Pair | AnyType => AnyType
                case x => ErrorLogger.err("cannot evaluate snd on non-pair type: " + x)
            } 
        }

        /* Returns the type of an lvalue (identifier, arrayElem, pairElem) */
        def getLValType(lVal: LValue): Type = {
            lVal match {
                case Ident(id) => getTypeFromVars(id, vars, childVars)                               
                case ArrayElem(id, _) => getTypeFromVars(id, vars, childVars) match {
                    case ArrayType(t) => t
                    /* Error if a non array ident is being accessed. */
                    case _ => ErrorLogger.err("unable to access non-array var as an array") // TODO : check :(
                }
                case x: PairElem => getLValPairElem(x)
            }
        }
            
        /* Returns the type of an rvalue. */    
        def getRValType(rval: RValue): Type = {

            rval match {
                case x: PairElem => getLValPairElem(x)
                
                case ArrayLiteral(xs) => {
                    if (xs.length > 0) {
                        
                        val head::tail = xs
                        val t = getRValType(head)

                        /* Error if not all array elements are the same type as the first. */
                        tail.foreach(exp => if (getRValType(exp) != t) ErrorLogger.err("Types in array not the same"))

                        ArrayType(getRValType(xs.head))
                    } else {
                        new ArrayType(AnyType)
                    }
                }

                case NewPair(fst, snd) => {
                    def getPairElem(rval: RValue): PairElemType = getPairElemType(getRValType(rval)) match {
                        case x: PairElemType => x
                        case x => ErrorLogger.err("not a pair elem type. expected: <? extends PairElemType>, actual: " + x) 
                    } 
                    return new PairType(getPairElem(fst), getPairElem(snd))
                }
                case Call(id, args) => {
                    val currentArgs = funcArgs.get(id) match {
                        case Some(x) => x
                        case _ => List()
                    }
                    if (args.length != currentArgs.length) ErrorLogger.err("Invalid number of arguments for function '" + id + "'. expected: " + currentArgs.length + ". actual: " + args.length)
                    for (i <- 0 to args.length - 1) {
                        val expArgType = currentArgs(i)
                        val actArgType = getRValType(args(i))
                        if (actArgType != expArgType) ErrorLogger.err("Invalid type for arg.  expected: " + expArgType + ". actual: " + actArgType)
                    }

                    return getTypeFromVars(id, vars)
                }
                case x: Expr => x match {
                    case _: IntLiteral => IntType
                    case _: CharLiteral => CharType
                    case _: StrLiteral => StringType
                    case _: BoolLiteral => BoolType
                    case PairLiteralNull => Pair
                    case Ident(id) => getTypeFromVars(id, vars, childVars)
                    case ArrayElem(id, exps) => {

                        def checkArrayIndex(exps: List[Expr], t: Type): Type = {
                            val head::tail = exps
                            val expType = getRValType(head)
                            if (expType != IntType) ErrorLogger.err("cannot access non-int type index for an array")
                            t match {
                                case ArrayType(subType) => if (tail.isEmpty) {
                                    return subType
                                } else {
                                    checkArrayIndex(tail, subType)
                                }
                                case _ if (!tail.isEmpty) => ErrorLogger.err("Array index out of bounds")
                                case x => x
                            }

                        }

                        getTypeFromVars(id, vars, childVars) match {
                            case ArrayType(t) => checkArrayIndex(exps, t)
                            case x => ErrorLogger.err("cannot get elem from non-array type")
                        }

                    }
                    case ArrayElem(_, Nil) => ErrorLogger.err("cannot have array elem with no expr")
                    case UnaryOpExpr(op, exp) => { 
                        val types = op match {
                            case Not => (BoolType, BoolType)
                            case Negate => (IntType, IntType)
                            case Ord => (CharType, IntType)
                            case Chr => (IntType, CharType)
                            case Length => (ArrayType(AnyType), IntType)
                        }
                        val rType = getRValType(exp)
                        if (rType != types._1) ErrorLogger.err("invalid type for unary op param. expected: " + types._1 + ". actual: " + rType)
                        return types._2
                    }
                    case BinaryOpExpr(op, exp1, exp2) => {
                        def checkType(exp: Expr, ReturnType: Type): Unit = {
                            if (ReturnType == AnyType) return
                            getRValType(exp) match {
                                case ReturnType => 
                                case _ => ErrorLogger.err("invalid binary op type")
                            }
                        }
                        val returnType = op match {
                            case Mul | Div | Mod | Add | Sub => (IntType, IntType, IntType)
                            case Equal | NotEqual => (AnyType, AnyType, BoolType)
                            case And | Or => (BoolType, BoolType, BoolType)
                            case Greater | GreaterEquals | Less | LessEquals => {
                                val t1 = getRValType(exp1)
                                val t2 = getRValType(exp2)

                                def validType(t: Type) = CharType == t || IntType == t

                                if (validType(t1) && validType(t2) && t1 == t2) return BoolType
                                else ErrorLogger.err("invalid equality type for binary op")
                            }
                        }
                        checkType(exp1, returnType._1)
                        checkType(exp2, returnType._2)
                        returnType._3
                    }
                    case _ => ErrorLogger.err("unknown")
                }
            }
        }

        /* Checks each statement */
        
        statements.foreach(statement => {
            statement match {
                case Declare(t, id, rhs) => {
                    val rType = getRValType(rhs)
                    if (rType != t) ErrorLogger.err("invalid type for declare. expected: " + t  + ", actual: " + rType)
                    declareVar(id, t, childVars)
                }
                case Assign(x, y) => {
                    /* Throw error if the ident being reassigned is a function. */
                    x match {
                        case Ident(id) if (funcArgs.keySet.exists(_ == id) && !childVars.keySet.exists(_ == id)) => ErrorLogger.err("Cannot re-assign value for a function: " + id) 
                        case _ => 
                    }
                    val lType = getLValType(x)
                    val rType = getRValType(y)
                    /* Throw error when attempting to assign type deleted pair to a type deleted pair */
                    if (lType == AnyType && rType == AnyType) ErrorLogger.err("Assignment is not legal when both sides types are not known. ltype: " + lType + ",rtype: " + rType)        
                    /* Throw error when attempting to assign to a different type */
                    if (lType != rType && rType != lType) ErrorLogger.err("invalid type for assign. expected : " + lType + ", actual : " + rType)              
                }
                case Read(x) => {
                    val ltype = getLValType(x)
                    /* Throw error if attempt to read to non int or char type. */
                    if (ltype != IntType && ltype != CharType) ErrorLogger.err("invalid type for read. expected: <IntType, CharType>, actual: " + ltype)  
                }
                case Free(x) => {
                    val rType = getRValType(x)
                    /* Error when freeing a non array or pair type. */
                    rType match {
                        case x: ArrayType => 
                        case x: PairType =>
                        case x => ErrorLogger.err("invalid type for free. expected: <Array, Pair>, actual: " + rType)
                    }
                }
                case Return(x) => {
                    val rVal = getRValType(x)
                    val rType = getTypeFromVars("\\func", vars, childVars)
                    if (rVal != rType) ErrorLogger.err("invalid type for return. expected: IntType, actual: " + rVal)
                }
                case Exit(x) => {
                    if (getRValType(x) != IntType) ErrorLogger.err("invalid type for exit")              
                }
                case Print(x) => getRValType(x)
                case Println(x) => getRValType(x)
                case If(p, xs, ys) => {
                    /* Error if condition not boolean type */
                    if (getRValType(p) != BoolType) ErrorLogger.err("invalid type for if cond") 

                    /* Check semantics of both branches. */
                    val newChildVars = createChildVars(vars, childVars)
                    checkStatements(xs, newChildVars, funcArgs)
                    checkStatements(ys, newChildVars, funcArgs)
                }
                case While(p, xs) => {
                    if (getRValType(p) != BoolType) ErrorLogger.err("invalid type for while cond") 
                    checkStatements(xs, createChildVars(vars, childVars), funcArgs)
                }
                case Begin(xs) => checkStatements(xs, createChildVars(vars, childVars), funcArgs)
                case default => 
            }
        })
    }

    def createChildVars(parent: Map[String, Type], child: MapM[String, Type]): Map[String, Type] = child.toMap ++ parent
    
}