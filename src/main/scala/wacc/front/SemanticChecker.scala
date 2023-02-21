package wacc.front

object SemanticChecker {

  import error._
  import wacc.ast._
  import wacc.back.SymbolTable

  import scala.collection.mutable.{ArrayBuffer, Map => MapM}

  def typecheck(program: Program, symbolTable: SymbolTable): ArrayBuffer[TypeException] = {

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

    /* check if a variable already exists in scope, error if it does, and add it to the scope if it doesn't */
    def declareVar(id: String, t: Type, vars: MapM[String, Type], pos: (Int, Int)): Unit = {
      if (vars.keySet.exists(_ == id)) errors += new TypeException(message = "Cannot redeclare variable '" + id + "'", pos = Seq(pos))
      else {
        vars(id) = t
        symbolTable.add(id, t)
      }
    }

    /* checks for invalid semantics within a specific function */
    def checkFunction(func: Func, vars: Map[String, Type]): Unit = {
      val childVars = MapM[String, Type]()

      /* add each function identifier to the global scope map */
      func.args.foreach(param => declareVar(param.id, param.t, childVars, param.pos))

      /* add the type of the function to the front of childVars so we know what type to return */
      declareVar("\\func", func.fs._1, childVars, func.pos)

      /* check semantics of each statement in the function */
      checkStatements(func.stats, createChildVars(vars, childVars))
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
    def checkStatements(statements: List[Stat], vars: Map[String, Type]): Unit = {

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
          case (x@Ident(id)) => getTypeFromVars(id, vars, childVars, x.pos)

          /* if its an array element, then get it's type from the parent and child scope maps */
          case (elem@ArrayElem(id, xs)) => getTypeFromVars(id, vars, childVars, elem.pos) match {

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
          case (array@ArrayLiteral(xs)) => {

            /* if it isn't empty */
            if (!xs.isEmpty) {

              /* error if the types of all elements in the array are not the same */
              val head :: tail = xs
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
          case (func@Call(id, args)) => {

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
            case (x@Ident(id)) => getTypeFromVars(id, vars, childVars, x.pos)

            /* error for array element with no index */
            case (array@ArrayElem(_, Nil)) => ErrorLogger.err("invalid array access\n  no index provided", array.pos)

            /* for an array element with index : */
            case (elem@ArrayElem(id, exps)) => {

              /* check array index is an int, and that is isn't out of bounds */
              def checkArrayIndex(exps: List[Expr], t: Type): Type = {
                val head :: tail = exps
                val expType = getRValType(head)

                /* error if array index isn't an int */
                if (expType != IntType) ErrorLogger.err("cannot access non-int type index for an array", expType, IntType, head.pos)

                /* return current array sub-type if this is its final dimension,
                                or recursive if it has another dimension to be accessed */
                t match {
                  case ArrayType(subType) => {
                    if (tail.isEmpty) {
                      return t
                    } else {
                      checkArrayIndex(tail, subType)
                    }
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
              val rType = getRValType(exp)

              /* error if the type of its input expression isn't the same as its input type */
              if (rType != op.input) ErrorLogger.err("invalid type for unary op param", rType, op.input, exp.pos)

              /* return the output type */
              return op.output
            }

            /* for a binary operator : */
            case BinaryOpExpr(op, exp1, exp2) => {

              /* checks that the input expression has correct type for given binary operator's operand and returns the type */
              def getType(exp: Expr, types: Seq[Type]): Type = {
                val rValType = getRValType(exp)
                types.foreach(t => {
                  if (t == rValType) return rValType
                })
                ErrorLogger.err("invalid binary op type", rValType, types, exp.pos)
              }

              /* check both operands are of correct type */
              val t1 = getType(exp1, op.input)
              val t2 = getType(exp2, op.input)
              if (t1 != t2) ErrorLogger.err("invalid type for binary op\n  cannot execute binary op on two args of differing type", t2, t1, exp1.pos, exp2.pos)

              /* return the output type */
              return op.output
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
          declareVar(id, t, childVars, rhs.pos)
        }

        /* check assign statement */
        case Assign(x, y) => {

          /* error if the identifier being reassigned is a function. */
          x match {
            case (ident@Ident(id)) if (funcArgs.keySet.exists(_ == id) && !childVars.keySet.exists(_ == id)) => ErrorLogger.err("Cannot re-assign value for a function: " + id, ident.pos)
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
          checkStatements(xs, newChildVars)
          checkStatements(ys, newChildVars)
        }

        /* check while statement */
        case While(p, xs) => {

          /* error if while condition isn't of boolean type */
          val rtype = getRValType(p)
          if (rtype != BoolType) ErrorLogger.err("invalid type for while cond", rtype, BoolType, p.pos)

          /* check semantics of loop body's statements */
          checkStatements(xs, createChildVars(vars, childVars))
        }

        /* check begin statement, by checking the semantics of its body's statements */
        case Begin(xs) => checkStatements(xs, createChildVars(vars, childVars))

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

    /* make global scope map immutable */
    val varsImm = vars.toMap

    /* check semantics of all statements in the program */
    functions.foreach(func => checkFunction(func, varsImm))
    checkStatements(statements, varsImm)

    return errors
  }

}
