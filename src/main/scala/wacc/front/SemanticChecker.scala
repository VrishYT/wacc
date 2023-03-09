package wacc
package front

object SemanticChecker {

  import wacc._
  import error._
  import wacc.ast._

  import scala.collection.mutable.{ArrayBuffer}

  def typecheck(program: Program, symbolTable: SymbolTable): ArrayBuffer[TypeException] = {

    /* array of typeExceptions to pass to compiler */
    val errors = ArrayBuffer[TypeException]()

    /* lists of all statements and functions in program */
    val classes = program.classes
    val statements = program.stats
    val functions = program.fs

    /* create a map for the global scope, containing function identifiers to return types */
    val vars = symbolTable.declare("main")

    /* check if a variable already exists in scope, error if it does, and add it to the scope if it doesn't */
    def declareVar(id: String, t: Type, vars: Table, pos: (Int, Int), isPrivate: Boolean = false): Unit = {
      t match {
        case ClassType(class_id) if (symbolTable.classes.contains(id)) => {
          errors += new TypeException(message = s"Cannot declare variable '$id'\n - class '$class_id' is not defined", pos = Seq(pos))
        }
        case _ => {
          if (!vars.add(id, Symbol(t, isPrivate))) errors += new TypeException(message = "Cannot redeclare variable '" + id + "'", pos = Seq(pos))
        }
      }

    }
    
    /* Add each class to the symbol table */
    classes.foreach(c => {
      val members = ClassTable(c.class_id)

      /* if class exists give a semantic error */
      if (symbolTable.classes.contains(c.class_id)) {
        errors += new TypeException(message = s"Cannot redeclare class '${c.class_id}'", pos = Seq(c.pos))
      } else {
        symbolTable.classes(c.class_id) = members
      }

    })

    /* Add attributes and functions to the symbol table */
    classes.foreach(c => {
      symbolTable.classes.get(c.class_id) match {
        case Some(members) => {
          /* declare attributes of a class */
          c.decls.foreach(f => f.t match {
            case ClassType(id) if (id == c.class_id) => {
              errors += new TypeException(message = s"Cannot have instance of a class within a class", pos = Seq(f.pos))
            }
            case _ => declareVar(f.id, f.t, members, f.pos, f.isPrivate)
          })

          /* declare methods within a class */
          c.funcs.foreach(func => {
            // TODO: reduce code duplication with normal func defs
            /* check if the functions already exists in the map */
            if (members.contains(func.fs._2)) {
              /* error if the function has been declared more than once */
              errors += new TypeException(message = s"Cannot redeclare function '${func.fs._2}' in class '${c.class_id}'", pos = Seq(func.pos))
            } else {
              /* Add the function into the global scope */
              if (func.args.distinct.size != func.args.size) errors += new TypeException(message = "Cannot redeclare function parameters", pos = Seq(func.pos))
              else {
                val table = MethodTable(func.fs._2, func.args.map(_.t), func.fs._1, func.isPrivate, members)
                func.args.foreach(param => table.add(param.id, ParamSymbol(param.t)))
                members.addTable(func.fs._2, table)
              }
            }
          })
        }
        case None => ???
      }

    })

    functions.foreach(func => {

      /* check if the functions already exists in the map */
      if (symbolTable.contains(func.fs._2)) {

        /* error if the function has been declared more than once */
        errors += new TypeException(message = "Cannot redeclare function '" + func.fs._2 + "'", pos = Seq(func.pos))
      } else {
        /* Add the function into the global scope */
        if (func.args.distinct.size != func.args.size) errors += new TypeException(message = "cannot redeclare function parameters", pos = Seq(func.pos))
        else symbolTable.declare(func.fs._2, func.args, func.fs._1)
      }
    })

    /* return the type of an identifier from the parent and child scope maps */
    def getTypeFromVars(id: String, vars: Table, pos: (Int, Int)): Type = {
      def get(vars: Table): Option[Type] = vars.getType(id) match {
        case x: Some[_] => x
        case None => None
      }

      vars match {
        case x: MethodTable => get(x) match {
          case Some(x) => x
          case _ => get(x.parent) match {
            case Some(x) => x
            case None => ErrorLogger.err("Variable/Class attribute " + id + " not found", pos)
          }
        }
        case _ => get(vars) match {
          case Some(x) => x
          case None => ErrorLogger.err("Variable " + id + " not found", pos)
        }
      }
    }

    /* traverse a list of statements and error on semantic errors */
    def checkStatements(statements: List[Stat], vars: Table): Unit = {

      def getClassType(ids: List[String], pos: (Int, Int)): Type = {

        def get(ident: String, elems: List[String], table: Table): Type = getTypeFromVars(ident, table, pos) match {
            case ClassType(class_id) => symbolTable.classes.get(class_id) match {
              case Some(x) => elems match {
                case Nil => ???
                case id :: Nil => x.getSymbol(id) match {
                  case Some(symbol) => {
                    if (symbol.isPrivate) {
                      ErrorLogger.err(s"cannot access private member '$id' of '$class_id'", pos)
                    } else symbol.t
                  }
                  case None => ErrorLogger.err(s"elem $id does not exist in class $class_id", pos)
                }
                case id :: elems => get(id, elems, x)
              }
              case None => ErrorLogger.err(s"class $class_id does not exist", pos)
            }
          case _ => ErrorLogger.err(s"$ident is not an instance of a class", pos)
        }

        get(ids.head, ids.tail, vars)

      }

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
          case (x@Ident(id)) => getTypeFromVars(id, vars, x.pos)

          /* if its an array element, then get it's type from the parent and child scope maps */
          case (elem@ArrayElem(id, xs)) => getTypeFromVars(id, vars, elem.pos) match {

            /* check if the type is an array type */
            case ArrayType(t) => t

            /* error if a non array identifier is being accessed. */
            case x => ErrorLogger.err("unable to access non-array var as an array", x, ArrayType(AnyType), elem.pos)
          }

          /* if it's a pair element then get the type of x, which is in the form fst(y) or snd(y),
                    by calling getLValPairElem */
          case x: PairElem => getLValPairElem(x)

          case classElem@ClassElem(ids) => {
            getClassType(ids, classElem.pos)
          }

          /* Default case - should be unreachable. */
          case _ => ErrorLogger.err("Unknown lvalue passed in", 1)
        }
      }

      /* returns true if the lvalue isn't declared yet */
      def isInferredTypeDefinition(lVal: LValue): Boolean = {
        lVal match {

          /* if its an identifier then check if its in the parent and child scope maps yet */
          case (x@Ident(id)) =>  vars.getType(id) match {
                                      case Some(x) => false
                                      case None => true
                               }
          case _ => false
        }
      }

      /* returns true if the lvalue has no type yet */
      def isTypelessParam(lVal: LValue): Boolean = {
        lVal match {

          /* if its an identifier then check if it has a type in the parent and child scope maps yet*/
          case (x@Ident(id)) =>  vars.getType(id) match {
                                      case Some(x) => {
                                        return x == NoType
                                      }
                                      case None => ???
                               }
          case _ => false
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
              tail.foreach(exp => if (getRValType(exp) != t) ErrorLogger.err("types in array not the same", getRValType(exp), t, array.pos))

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

          case classElem@ClassElem(ids) => {
            getClassType(ids, classElem.pos)
          }

          case newClass@NewClass(class_id, rvals) => {
            val class_mems = symbolTable.classes.get(class_id) match {
              case Some(x) => x
              case None => ErrorLogger.err(s"invalid constructor for class ${class_id}\n  - class '${class_id}' does not exist", newClass.pos)
            }

            val class_types = class_mems match {
              case z : ClassTable => z.types()
              case _ => ???
            }

            val size = class_mems.getSize
            if (rvals.length != size){
              ErrorLogger.err(s"invalid constructor for class ${class_id}\n  - missing arguments", newClass.pos)
            }

            // TODO: REPLACE FOR LOOP WITH FOREACH
            for (i <- 0 to size - 1) {
              val expectedType = class_types(i) 
              val field = rvals(i)
              val rValType = getRValType(field) 
              if (expectedType != rValType) ErrorLogger.err(s"invalid constructor for class ${class_id}\n  - invalid type of argument", expectedType, rValType, field.pos)
            }
              
            return new ClassType(class_id)
          }


          /* if it's a function call :  */
          case (func@Call(ids, args)) => {
            if (ids.length == 1) {
                /* get a list of its parameter types in order */
              val funcVars = symbolTable.get(ids.head) match {
                case Some(x) => x
                case None => ErrorLogger.err(s"function '${ids}' is undefined", func.pos) 
              }
              val currentArgs = funcVars.paramTypes

              /* error if the number of arguments is wrong */
              if (args.length != currentArgs.length) ErrorLogger.err("invalid number of arguments for function '" + ids + "'. expected: " + currentArgs.length + ". actual: " + args.length, func.pos)

              /* check each argument type passed in is the same as the corresponding parameter for this function */
              for (i <- 0 to args.length - 1) {
                val paramType = currentArgs(i)
                val rType = getRValType(args(i))

                /* error an argument type doesn't match the required parameter */
                if (rType != paramType) ErrorLogger.err("invalid type for arg", rType, paramType, args(i).pos)
              }

              /* return the type of the function from the identifier maps */
              return funcVars.returnType
            }
            return IntType
            
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
            case (x@Ident(id)) => getTypeFromVars(id, vars, x.pos)

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
              getTypeFromVars(id, vars, elem.pos) match {
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
              import scala.annotation.nowarn

              /* checks that the input expression has correct type for given binary operator's operand and returns the type */
              @nowarn def getType(exp: Expr, types: Seq[Type]): Type = {
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
          declareVar(id, t, vars, rhs.pos)
        }

        /* check assign statement */
        case AssignOrTypelessDeclare(x, y) => {/* get type of left and right hand sides of the assign */

          val rType = getRValType(y)
          var lType = rType

          /* error if the identifier being reassigned is a function. */
          x match {
            case (ident@Ident(id)) => {
              if (symbolTable.contains(id) && !vars.contains(id)) {
                ErrorLogger.err("Cannot re-assign value for a function: " + id, ident.pos)
              } else if (isInferredTypeDefinition(x)) {
                declareVar(id, rType, vars, y.pos)
              } else if (isTypelessParam(x)) {
                vars.updateRecursive(id, Symbol(rType))
              } else {
                lType = getLValType(x)
              }
            }
            case _ => lType = getLValType(x)
          }

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
          val funcTable = vars match {
            case x: FuncTable if (x.id != "main") => x
            case _ => ErrorLogger.err("invalid return call\n  cannot return outside a function body", x.pos)
          }

          /* error if return type does not match return type of the current function being checked */
          var funcType = funcTable.getReturnType

          if (funcType == NoType) {
            funcType = rType
            funcTable.setReturnType(rType)
          }

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
          val thenVars = ChildTable(vars)
          checkStatements(xs, thenVars)
          val elseVars = ChildTable(vars)
          checkStatements(ys, elseVars)
          vars.addIf(thenVars, elseVars)
        }

        /* check while statement */
        case While(p, xs) => {

          /* error if while condition isn't of boolean type */
          val rtype = getRValType(p)
          if (rtype != BoolType) ErrorLogger.err("invalid type for while cond", rtype, BoolType, p.pos)

          /* check semantics of loop body's statements */
          val child = ChildTable(vars)
          checkStatements(xs, child)
          vars.addWhile(child)
        }

        /* check begin statement, by checking the semantics of its body's statements */
        case Begin(xs) => {
          val child = ChildTable(vars)
          checkStatements(xs, child)
          vars.addBegin(child)
        }

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

    /* check semantics of all statements in the program */
    classes.foreach(c => symbolTable.classes.get(c.class_id) match {
      case Some(members) => c.funcs.foreach(func => members.getMethodTable(func.fs._2) match {
        case Some(x) => checkStatements(func.stats, x)
        case None => {
          errors += new TypeException(message = s"invalid method declaration in '${c.class_id}'", pos = Seq(func.pos))
        }
      }) 
      case None =>  
    })

    functions.foreach(func => symbolTable.get(func.fs._2) match {
      case Some(x) => checkStatements(func.stats, x)
      case None => errors += new TypeException(message = "Invalid function declaration", pos = Seq(func.pos))
    })

    checkStatements(statements, vars)

    return errors
  }

}
