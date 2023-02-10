package wacc

import AST._

object SemanticChecker {

    import scala.collection.immutable.Map
    import scala.collection.mutable.{Map => MapM}
    import scala.collection.mutable.ArrayBuffer
    import error._

    def typecheck(program: Program): ArrayBuffer[TypeException] = {
        /* array of typeExceptions to pass to compiler */
        val errors = ArrayBuffer[TypeException]()

        /* lists of all statements and functions in program */
        val statements = program.stats
        val functions = program.fs

        /* create a map for the global scope, containing function identifiers to return types */
        val vars = MapM[String, Type]()

        /* create a map for functions to an ordered list of their parameter types */
        val funcArgs = MapM[String, List[Type]]()
        functions.foreach(func => {

            /* check if the functions already exists in the map */
            if (vars.keySet.exists(_ == func.fs._2)) {

                /* error if the function has been declared more than once */
                errors += new TypeException(message = "Cannot redeclare function '" + func.fs._2 + "'", pos = Seq(func.pos))
            } else {

                /* Add the function into the global scope */
                vars(func.fs._2) = func.fs._1
                /* Add the function into the map of functions to parameter types */
                funcArgs(func.fs._2) = func.args.map(arg => arg.t)
            }
        })

        /* make global scope map and function parameter type map immutable */
        val varsImm = vars.toMap
        val funcArgsImm = funcArgs.toMap

        /* check semantics of all statements in the program */
        functions.foreach(func => checkFunction(func, varsImm, funcArgsImm, errors))
        checkStatements(statements, varsImm, funcArgsImm, errors)
        
        return errors
    }

    /* check if a variable already exists in scope, error if it does, and add it to the scope if it doesn't */
    def declareVar(id: String, t: Type, vars: MapM[String, Type], pos: (Int, Int), errors: ArrayBuffer[TypeException]): Unit = {
        if (vars.keySet.exists(_ == id)) errors += new TypeException(message = "Cannot redeclare variable '" + id + "'", pos = Seq(pos)) 
        else vars(id) = t
    }

    /* checks for invalid semantics within a specific function */
    def checkFunction(func: Func, vars: Map[String, Type], funcArgs: Map[String, List[Type]], errors: ArrayBuffer[TypeException]): Unit = {
        val childVars = MapM[String, Type]()

        /* add each function identifier to the global scope map */
        func.args.foreach(param => declareVar(param.id, param.t, childVars, param.pos, errors))

        /* add the type of the function to the front of childVars so we know what type to return */
        declareVar("\\func", func.fs._1, childVars, func.pos, errors) 

        /* check semantics of each statement in the function */
        checkStatements(func.stats, createChildVars(vars, childVars), funcArgs, errors)
    }

    /* return the type of an identifier from the parent and child scope maps */
    def getTypeFromVars(id: String, parent: Map[String, Type], child: MapM[String, Type] = MapM(), pos: (Int, Int)): Type = {
        child.get(id) match {
            case Some(x) => x
            case None => parent.get(id) match {
                case Some(x) => x
                case None => ErrorLogger.err("Variable " + id + " not found", pos)
            }
        }
    }

    /* traverse a list of statements and error on semantic errors */
    def checkStatements(statements: List[Stat], vars: Map[String, Type], funcArgs: Map[String, List[Type]], errors: ArrayBuffer[TypeException]): Unit = {

        /* create a new, empty child scope */
        val childVars = MapM[String, Type]()

        /* returns the type of the contents of a pairElem (fst(x) or snd(x), where x is passed in) */
        def getPairElemType(t: Type): Type = t match {
            case x: PairType => Pair
            case x => x
        }

        /* returns the type of a pairElem (fst(x) or snd(x)) */
        def getLValPairElem(p: PairElem) = p match {
            case Fst(x) => getLValType(x) match {
                case PairType(fst, _) => getPairElemType(fst)
                case _ => AnyType
            } 
            case Snd(x) => getLValType(x) match {
                case PairType(_, snd) => getPairElemType(snd)
                case _ => AnyType
            } 
        }

        /* returns the type of an lvalue */
        def getLValType(lVal: LValue): Type = {
            lVal match {

                /* if its an identifier then get it's type from the parent and child scope maps */
                case (x @ Ident(id)) => getTypeFromVars(id, vars, childVars, x.pos)   

                /* if its an array element, then get it's type from the parent and child scope maps */                            
                case (elem @ ArrayElem(id, xs)) => getTypeFromVars(id, vars, childVars, elem.pos) match {

                    /* check if the type is an array type */
                    case ArrayType(t) => t

                    /* error if a non array identifier is being accessed. */
                    case x => ErrorLogger.err("unable to access non-array var as an array", x, ArrayType(AnyType), elem.pos)
                }

                /* if it's a pair element then get the type of x, which is in the form fst(y) or snd(y), 
                   by calling getLValPairElem */
                case x: PairElem => getLValPairElem(x)
                /* Default case - should be unreachable. */
                case _ => ErrorLogger.err("Unknown lvalue passed in", 1)
            }
        }
            
        /* return the type of an rvalue. */    
        def getRValType(rval: RValue): Type = {

            rval match {

                /* if it's a pair element then get the type of x, which is in the form fst(y) or snd(y), 
                   by calling getLValPairElem */
                case x: PairElem => getLValPairElem(x)

                /* if it's an array literal then : */
                case (array @ ArrayLiteral(xs)) => {

                    /* if it isn't empty */
                    if (!xs.isEmpty) {

                        /* error if the types of all elements in the array are not the same */
                        val head::tail = xs
                        val t = getRValType(head)
                        tail.foreach(exp => if (getRValType(exp) != t) ErrorLogger.err("Types in array not the same", getRValType(exp), t, array.pos))

                        /* return ArrayType of the type of elements in the array */
                        ArrayType(getRValType(xs.head))
                    } else {

                        /*  return ArrayType of any type */
                        new ArrayType(AnyType)
                    }
                }

                /* if it's a pair constructor, return a PairType of the types of each of its elements */
                case NewPair(fst, snd) => {
                    def getPairElem(rval: RValue): Type = getPairElemType(getRValType(rval)) 
                    return new PairType(getPairElem(fst), getPairElem(snd))
                }

                /* if it's a function call :  */
                case (func @ Call(id, args)) => {

                    /* get a list of its parameter types in order */
                    val currentArgs = funcArgs.get(id) match {
                        case Some(x) => x
                        case _ => List()
                    }

                    /* error if the number of arguments is wrong */
                    if (args.length != currentArgs.length) ErrorLogger.err("Invalid number of arguments for function '" + id + "'. expected: " + currentArgs.length + ". actual: " + args.length, func.pos)

                    /* check each argument type passed in is the same as the corresponding parameter for this function */
                    for (i <- 0 to args.length - 1) {
                        val paramType = currentArgs(i)
                        val rType = getRValType(args(i))

                        /* error an argument type doesn't match the required parameter */
                        if (rType != paramType) ErrorLogger.err("invalid type for arg", rType, paramType, args(i).pos)
                    }

                    /* return the type of the function from the identifier maps */
                    return getTypeFromVars(id, vars, pos = func.pos)
                }

                /* for an expression, match on the specific type of expression : */
                case x: Expr => x match {

                    /* for atomic types, return their corresponding type */
                    case _: IntLiteral => IntType
                    case _: CharLiteral => CharType
                    case _: StrLiteral => StringType
                    case _: BoolLiteral => BoolType
                    case _: PairLiteralNull => Pair

                    /* for an identifier, get its type from the identifier maps */
                    case (x @ Ident(id)) => getTypeFromVars(id, vars, childVars, x.pos)

                    /* error for array element with no index */
                    case (array @ ArrayElem(_, Nil)) => ErrorLogger.err("invalid array access\n  no index provided", array.pos)

                    /* for an array element with index : */
                    case (elem @ ArrayElem(id, exps)) => {

                        /* check array index is an int, and that is isn't out of bounds */
                        def checkArrayIndex(exps: List[Expr], t: Type): Type = {
                            val head::tail = exps
                            val expType = getRValType(head)

                            /* error if array index isn't an int */
                            if (expType != IntType) ErrorLogger.err("cannot access non-int type index for an array", expType, IntType, head.pos)

                            /* return current array sub-type if this is its final dimension,
                               or recursive if it has another dimension to be accessed */
                            t match {
                                case ArrayType(subType) => if (tail.isEmpty) {
                                    return subType
                                } else {
                                    checkArrayIndex(tail, subType)
                                }

                                /* error if too many dimensions are specified */
                                case _ if (!tail.isEmpty) => ErrorLogger.err("array index out of bounds", elem.pos)

                                /* return current type if no more dimensions are specified */
                                case x => x
                            }
                        }

                        /* get the array's type from the variable maps, and error if it's a non-array type, or undefined */
                        getTypeFromVars(id, vars, childVars, elem.pos) match {
                            case ArrayType(t) => checkArrayIndex(exps, t)
                            case x => ErrorLogger.err("cannot get elem from non-array type", x, ArrayType(AnyType), elem.pos)
                        }
                    }

                    /* for a unary operator, get its input and output types as val types */
                    case UnaryOpExpr(op, exp) => { 
                        val types = op match {
                            case Not => (BoolType, BoolType)
                            case Negate => (IntType, IntType)
                            case Ord => (CharType, IntType)
                            case Chr => (IntType, CharType)
                            case Length => (ArrayType(AnyType), IntType)
                        }
                        val rType = getRValType(exp)

                        /* error if the type of its input expression isn't the same as its input type */
                        if (rType != types._1) ErrorLogger.err("invalid type for unary op param", rType, types._1, exp.pos)

                        /* return the output type */
                        return types._2
                    }

                    /* for a binary operator : */
                    case BinaryOpExpr(op, exp1, exp2) => {

                        /* checks that the input expression has correct type for given binary operator's operand */
                        def checkType(exp: Expr, ReturnType: Type): Unit = {
                            val rValType = getRValType(exp)
                            if (ReturnType == AnyType) return
                            rValType match {
                                case ReturnType => 
                                case _ => ErrorLogger.err("invalid binary op type", rValType, ReturnType, exp.pos)
                            }
                        }

                        /* define returnType as specific operator's tow operand types and output type  */
                        val returnType = op match {
                            case Mul | Div | Mod | Add | Sub => (IntType, IntType, IntType)
                            case Equal | NotEqual => (AnyType, AnyType, BoolType)
                            case And | Or => (BoolType, BoolType, BoolType)
                            case Greater | GreaterEquals | Less | LessEquals => {
                                val t1 = getRValType(exp1)
                                val t2 = getRValType(exp2)

                                /* define valid types for greater(equals) and less(equals) as integers and characters */
                                def validType(t: Type) = CharType == t || IntType == t

                                /* error if the type of either operand is invalid */
                                if (!validType(t1)) ErrorLogger.err("invalid type for binary op", t1, Seq(CharType, IntType), exp1.pos)
                                if (!validType(t2)) ErrorLogger.err("invalid type for binary op", t2, Seq(CharType, IntType), exp2.pos)

                                /* error if the type of the operands are not the same */
                                if (t1 != t2) ErrorLogger.err("invalid type for binary op\n  cannot execute binary op on two args of differing type", t2, t1, exp1.pos, exp2.pos)

                                /* return boolean type */
                                return BoolType
                            }
                        }

                        /* check both operands are of correct type */
                        checkType(exp1, returnType._1)
                        checkType(exp2, returnType._2)

                        /* return the output type */
                        returnType._3
                    }
                    /* Default case - should be unreachable. */
                    case _ => ErrorLogger.err("Unknown expression passed in", 1)
                }
            }
        }

        /* checks each statement */
        def checkStatement(statement: Stat): Unit = statement match {

            /* check declare statement */
            case Declare(t, id, rhs) => {
                val rType = getRValType(rhs)

                /* error if the left type is not the same as the right type */
                if (rType != t) ErrorLogger.err("invalid type for declare", rType, t, rhs.pos)

                /* check identifier hasn't already been declared, and add it to the scope */
                declareVar(id, t, childVars, rhs.pos, errors)
            }

            /* check assign statement */
            case Assign(x, y) => {

                /* error if the identifier being reassigned is a function. */
                x match {
                    case (ident @ Ident(id)) if (funcArgs.keySet.exists(_ == id) && !childVars.keySet.exists(_ == id)) => ErrorLogger.err("Cannot re-assign value for a function: " + id, ident.pos) 
                    case _ => 
                }

                /* get type of left and right hand sides of the assign */
                val lType = getLValType(x)
                val rType = getRValType(y)

                /* error when attempting to assign an unknown type to another unknown type */
                if (lType == AnyType && rType == AnyType) ErrorLogger.err("invalid type for assign\n  cannot assign when both types are unknown", x.pos, y.pos)        

                /* error when attempting to assign to a different type */
                if (lType != rType && rType != lType) ErrorLogger.err("invalid type for assign", rType, lType, x.pos, y.pos)        
            }

            /* check read statement */
            case Read(x) => {
                val ltype = getLValType(x)

                /* error if attempting to read to non int or char type. */
                if (ltype != IntType && ltype != CharType) ErrorLogger.err("invalid type for read", ltype, Seq(IntType, CharType), x.pos)
            }

            /* check free statement */
            case Free(x) => {
                val rType = getRValType(x)

                /* error when freeing a non array or pair type. */
                rType match {
                    case x: ArrayType => 
                    case x: PairType =>
                    case y => ErrorLogger.err("invalid type for free", y, Seq(ArrayType(AnyType), PairType(AnyType, AnyType)), x.pos)
                }
            }
            
            /* check return statement */
            case Return(x) => {
                val rType = getRValType(x)

                /* error if we are not inside of a function */
                if (!vars.keySet.exists(_ == "\\func")) ErrorLogger.err("invalid return call\n  cannot return outside a function body", x.pos)

                /* error if return type does not match return type of the current function being checked */
                val funcType = getTypeFromVars("\\func", vars, childVars, x.pos)
                if (rType != funcType) ErrorLogger.err("invalid type for return", rType, funcType, x.pos)
            }

            /* check exit statement */
            case Exit(x) => {

                /* error if exit code is not an intType */
                val rValType = getRValType(x) 
                if (rValType != IntType) ErrorLogger.err("invalid type for exit", rValType, IntType, x.pos)
            }

            /* check print statement */
            case Print(x) => getRValType(x)

            /* check println statement */
            case Println(x) => getRValType(x)

            /* check if statement */
            case If(p, xs, ys) => {

                /* error if condition not of boolean type */
                val rValType = getRValType(p) 
                if (rValType != BoolType) ErrorLogger.err("invalid type for if cond", rValType, BoolType, p.pos) 

                /* check semantics of both branches of if statement */
                val newChildVars = createChildVars(vars, childVars)
                checkStatements(xs, newChildVars, funcArgs, errors)
                checkStatements(ys, newChildVars, funcArgs, errors)
            }

            /* check while statement */
            case While(p, xs) => {

                /* error if while condition isn't of boolean type */
                val rtype = getRValType(p)
                if (rtype != BoolType) ErrorLogger.err("invalid type for while cond", rtype, BoolType, p.pos) 

                /* check semantics of loop body's statements */
                checkStatements(xs, createChildVars(vars, childVars), funcArgs, errors)
            }

            /* check begin statement, by checking the semantics of its body's statements */
            case Begin(xs) => checkStatements(xs, createChildVars(vars, childVars), funcArgs, errors)

            /* defualt case */
            case default => 
        }

        /* check each statement in the program, and add any TypeExceptions to the list of errors */
        statements.foreach(statement => try {
            checkStatement(statement)
        } catch {
            case x: TypeException => errors += x
        })
    }

    /* create a new parent scope by combining the current parent and child identifier maps */
    def createChildVars(parent: Map[String, Type], child: MapM[String, Type]): Map[String, Type] = child.toMap ++ parent
    
}