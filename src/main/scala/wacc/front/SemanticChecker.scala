package wacc
package front

object SemanticChecker {

  import wacc._
  import error._
  import wacc.ast._

  import scala.collection.mutable.ArrayBuffer
  import scala.collection.mutable.LinkedHashMap

  def typecheck(program: Program, symbolTable: SymbolTable): ArrayBuffer[TypeException] = {

    /* array of typeExceptions to pass to compiler */
    val errors = ArrayBuffer[TypeException]()

    /* lists of all statements and functions in program */
    val classes = program.classes
    val statements = program.stats
    val functions = program.fs

    /* create a map for the global scope, containing function identifiers to return types */
    val vars = symbolTable.declare("_main")

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
      /* if class exists give a semantic error */
      if (symbolTable.classes.contains(c.class_id)) {
        errors += new TypeException(message = s"Cannot redeclare class '${c.class_id}'", pos = Seq(c.pos))
      } else {
        val types = c.decls.map(_.t)
        val members = ClassTable(c.class_id, types)
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
            val funcId = func.fs._2

            members.getOverloadCount(funcId) match {
              case Some(count) => {
                if (checkNonDuplicateMethod(func, count)) {
                  storeMethod(func, count)
                  members.setOverload(funcId, count + 1)
                }
              }
              case None => {
                storeMethod(func, 0)
                members.setOverload(funcId, 1)
              }
            }
          })

          def storeMethod(func: Func, i: Int): Unit = {
            if (func.args.distinct.size != func.args.size) {
              errors += new TypeException(message = "Cannot redeclare function parameters", pos = Seq(func.pos))
            } else {
              val uniqueMethodId = s"${i}_${func.fs._2}"
              // val uniqueFuncId = s"${c.class_id}_${i}_${func.fs._2}"

              val pairs = func.args.map(_.id) zip func.args.map(_.t)
              val map = LinkedHashMap[String, Type]()
              pairs.foreach(pair => map(pair._1) = pair._2)
              val table = MethodTable(uniqueMethodId, map, func.fs._1, func.isPrivate, members)

              func.args.foreach(param => table.add(param.id, ParamSymbol(param.t)))
              /* modify arguments to take a class instance as a parameter */
              func.args.prepend(TypedParam(ClassType(c.class_id), "this")(0,0))
              table.add("this", ParamSymbol(ClassType(c.class_id)))
              members.addTable(uniqueMethodId, table)
              func.rename(uniqueMethodId)
            }
          }

          def checkNonDuplicateMethod(func: Func, count: Int): Boolean = {
            (0 until count).foreach(i => {
              val funcTable = members.getMethodTable(s"${i}_${func.fs._2}") match {
                case Some(x) => x
                case None => ???
              }
              /* error if the function has been declared more than once */
              if (funcTable.returnType == func.fs._1 && (funcTable.paramIdTypes.values.toSeq) == func.args.map(_.t)) {
                errors += new TypeException(message = "Cannot redeclare function '" + func.fs._2 + "'", pos = Seq(func.pos))
                return false
              }
            })
            return true
          }
        }
        case None => ???
      }

    })

    functions.foreach(func => {

      /* Add the function into the global scope */
      def storeFunction(func: Func, i: Int): Unit = {
        val uniqueFuncId = s"${i}_${func.fs._2}"
        if (func.args.distinct.size != func.args.size) {
          errors += new TypeException(message = "Cannot redeclare function parameters", pos = Seq(func.pos))
        } else {
          symbolTable.declare(uniqueFuncId, func.args.toSeq, func.fs._1)
          func.rename(uniqueFuncId)
        }

        symbolTable.get(uniqueFuncId) match {
          case Some(x) => {
            func.stats.foreach(stat => try {
              tryInferParam(stat, x)
            } catch {
              case x: TypeException =>
            })
            x.paramIdTypes.keys.toSeq.foreach(id => {
              if (x.paramIdTypes(id) == NoType) {
                x.paramIdTypes(id) = AnyType
              }
            })
          }
          case None => 
        }
      }


      /* check a function is not a duplicate function */
      def checkNonDuplicateFunction(func: Func, count: Int): Boolean = {
        (0 until count).foreach(i => {
          val funcTable = symbolTable.get(s"${i}_${func.fs._2}") match {
            case Some(x) => x
            case None => ???
          }
          /* error if the function has been declared more than once */
          if (funcTable.returnType == func.fs._1 && (funcTable.paramIdTypes.values.toSeq) == func.args.map(_.t)) {
            errors += new TypeException(message = "Cannot redeclare function '" + func.fs._2 + "'", pos = Seq(func.pos))
            return false
          }
        })
        return true
      }

      val id = func.fs._2

      symbolTable.getOverloadCount(id) match {
        case Some(count) => {
          if (checkNonDuplicateFunction(func, count)) {
            storeFunction(func, count)
            symbolTable.setOverload(id, count + 1)
          }
        }
        case None => {
          storeFunction(func, 0)
          symbolTable.setOverload(id, 1)
        }
      }
    })

    /* return the type of an identifier from the parent and child scope maps */
    def getTypeFromVars(id: String, vars: Table, pos: (Int, Int), updateRefs: Boolean = true): Type = {
      def get(vars: Table): Option[Type] = vars.getSymbol(id) match {
        case x@Some(y) => {
          if (updateRefs){
            y.refCount += 1
          }
          return Some(y.t)
        }
        case None => vars.getType("this") match {
          case Some(x) => x match {
            case ClassType(class_id) => symbolTable.classes.get(class_id) match {
              case Some(x) => x.getType(id)
              case None => ???
            }
            case _ => ??? 
          }
          case None => vars match {
            case x: MethodTable => if (id == "this") Some(ClassType(x.parent.id)) else get(x.parent)
            case x: ChildTable => get(x.parent)
            case _ => None
          }
        }
      }
      
      get(vars) match {
        case Some(x) => x
        case None => ErrorLogger.err("Variable " + id + " not found", pos)
      }
    }

    def getFuncTable(table: Table, rVal: RValue): FuncTable = table match {
            case x: FuncTable if (x.id != "_main") => x
            case x: ChildTable => getFuncTable(x.parent, rVal)
            case _ => ErrorLogger.err("invalid return call\n  cannot return outside a function body", rVal.pos)
          }

    def getClassType(ids: List[String], pos: (Int, Int), vars: Table, updateRefs: Boolean = false): Type = {

      def get(ident: String, elems: List[String], table: Table): Type = getTypeFromVars(ident, table, pos, updateRefs) match {
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
    def getLValPairElem(p: PairElem, vars: Table, updateRefs: Boolean = false) = p match {
      case Fst(x) => getLValType(x, vars, updateRefs) match {
        case PairType(fst, _) => getPairElemType(fst)
        case _ => AnyType
      }
      case Snd(x) => getLValType(x, vars, updateRefs) match {
        case PairType(_, snd) => getPairElemType(snd)
        case _ => AnyType
      }
    }

    /* returns the type of an lvalue */
    def getLValType(lVal: LValue, vars: Table, updateRefs: Boolean = false): Type = {
      lVal match {

        /* if its an identifier then get it's type from the parent and child scope maps */
        case (x@Ident(id)) => getTypeFromVars(id, vars, x.pos, updateRefs)

        /* if its an array element, then get it's type from the parent and child scope maps */
        case (elem@ArrayElem(id, xs)) => getTypeFromVars(id, vars, elem.pos, updateRefs) match {

          /* check if the type is an array type */
          case ArrayType(t) => t

          /* error if a non array identifier is being accessed. */
          case x => ErrorLogger.err("unable to access non-array var as an array", x, ArrayType(AnyType), elem.pos)
        }

        /* if it's a pair element then get the type of x, which is in the form fst(y) or snd(y),
           by calling getLValPairElem */
        case x: PairElem => getLValPairElem(x, vars, updateRefs)

        case classElem@ClassElem(ids) => {
          getClassType(ids, classElem.pos, vars)
        }

        /* Default case - should be unreachable. */
        case _ => ErrorLogger.err("Unknown lvalue passed in", 1)
      }
    }

    
    /* return the type of an rvalue. */
    def getRValType(vars: Table, rval: RValue, lval: (Boolean, Option[Type]) = (false, None), prevRVal: Option[Call] = None): Type = {

      checkParamRVal(rval, vars)

      rval match {
        /* if it's a pair element then get the type of x, which is in the form fst(y) or snd(y),
           by calling getLValPairElem */
        case x: PairElem => getLValPairElem(x, vars, true)

        /* if it's an array literal then : */
        case (array@ArrayLiteral(xs)) => {

        /* if it isn't empty */
          if (!xs.isEmpty) {

            /* error if the types of all elements in the array are not the same */
            val head :: tail = xs
            val t = getRValType(vars, head)
            tail.foreach(exp => if (getRValType(vars, exp) != t) ErrorLogger.err("types in array not the same", getRValType(vars, exp), t, array.pos))

            /* return ArrayType of the type of elements in the array */
            ArrayType(getRValType(vars, xs.head))
           } else {

            /*  return ArrayType of any type */
            new ArrayType(AnyType)
          }
        }

        /* if it's a pair constructor, return a PairType of the types of each of its elements */
        case NewPair(fst, snd) => {
          def getPairElem(rval: RValue): Type = getPairElemType(getRValType(vars, rval))

          return new PairType(getPairElem(fst), getPairElem(snd))
          }

        case classElem@ClassElem(ids) => {
          getClassType(ids, classElem.pos, vars, true)
        }

        case newClass@NewClass(class_id, rvals) => {
          val class_mems : ClassTable = symbolTable.classes.get(class_id) match {
            case Some(x) => {
              x.useCount += 1
              x
            }
            case None => ErrorLogger.err(s"invalid constructor for class ${class_id}\n  - class '${class_id}' does not exist", newClass.pos)
          }

          val class_types = class_mems match {
            case z : ClassTable => z.types
            case _ => ???
          }

          val size = class_mems.getSize()
          if (rvals.length != size){
            ErrorLogger.err(s"invalid constructor for class ${class_id}\n  - missing arguments", newClass.pos)
          }

          for (i <- 0 to size - 1) {
            val expectedType = class_types(i) 
            val field = rvals(i) match {
              case x: Expr => x
              case x => ErrorLogger.err(s"invalid constructor for class ${class_id}\n  - invalid type of argument: arguments must be an expression\n  - if an argument is a function/method call, pair element or new object (array, pair or class instance), declare a new variable first", x.pos)
            }
            val rValType = getRValType(vars, field)
            if (expectedType != rValType) ErrorLogger.err(s"invalid constructor for class ${class_id}\n  - invalid type of argument", expectedType, rValType, field.pos)
          }
              
          return new ClassType(class_id)
        }


        /* if it's a function call :  */
        case (func@Call(ids, args)) => {

          def getParentClass(table: Table): Option[ClassTable] = table match {
            case x: ClassTable => Some(x)
            case x: ChildTable => getParentClass(x.parent)
            case x: MethodTable => getParentClass(x.parent)
            case _ => None
          }

          // println(s"call ${ids.mkString(".")}(${args.mkString(",")})")
          // println(vars)

          def getFuncType(id: String, table: Either[SymbolTable, ClassTable]): Type = {

              /* error if function not defined */
              val count: Int = table match {
                case Left(x) => x.getOverloadCount(id) match {
                  case Some(x) => x 
                  case None => {
                    // println("check class")
                    return getRValType(vars, Call("this" +: ids, args)(func.pos), lval, Some(func))
                  }
                }
                case Right(x) => x.getOverloadCount(id) match {
                  case Some(x) => x // 
                  case None => ErrorLogger.err(s"Method '${id}' is undefined in class '${x.id}'", func.pos) 
                }
              }

              // println(count)

              /* check every overloaded function for a match */
              (0 until count).foreach(i => {
                // println(i)
                val uniqueFuncId = s"${i}_${id}"
                val funcVars = table match {
                  case Left(x) => x.get(uniqueFuncId) match {
                    case Some(x) => {
                      x.useCount += 1
                      x
                    }
                    case None => getParentClass(vars) match {
                      case Some(x) => x.getMethodTable(uniqueFuncId) match {
                        case Some(x) => {
                          x.useCount += 1
                          x
                        }
                        case None => ???
                      }
                      case None => ???
                    }
                  }
                  case Right(x) => x.getMethodTable(uniqueFuncId) match {
                    case Some(x) => {
                      x.useCount += 1
                      x
                    }
                    case None => ???
                  }
                }

                val verify = checkOverloadedFunc(funcVars)
                // println(verify)

                if (verify._1) {
                  func.rename(uniqueFuncId)
                  prevRVal match {
                    case Some(prevFunc) => prevFunc.rename(uniqueFuncId)
                    case None => 
                  }
                  return funcVars.returnType
                } else {
                  verify._2 match {
                    case Some(x) => ErrorLogger.err(s"invalid call to ${id}\n  - $x", func.pos)
                    case _ =>
                  }
                }              
              })

              // println("no")

              /* if none are valid, error */
              ErrorLogger.err(s"invalid call to ${id}\n  - no function with same return/parameter arguments", func.pos)
          }
          
          def checkOverloadedFunc(funcVars: FuncTable): (Boolean, Option[String]) = {

            val currentArgs = funcVars.paramIdTypes.values.toSeq

            /* error if the number of arguments is wrong */
            if (args.length != currentArgs.length) return (false, None)

            /* check args */
            for (i <- 0 to args.length - 1) {
              val paramType = currentArgs(i)
              val rType = getRValType(vars, args(i))

              /* error an argument type doesn't match the required parameter */
              if (rType != paramType) {
                if (paramType != NoType) {
                  return (false, None)
                }
                funcVars.paramIdTypes(funcVars.paramIdTypes.keys.toSeq(i)) = rType
              }
            }

            val matches = lval._2 match {
              case Some(x) => x == funcVars.returnType
              /* if lval._1 is true, this is a typeless declare, so takes the first match on arguments */
              case None => lval._1
            }

            if (matches && funcVars.isPrivate) {

              val isValid = funcVars match {
                case x: MethodTable => getParentClass(vars) match {
                  case Some(p) => p.id == x.parent.id
                  case _ => false
                }
                case _ => false
              }

              (isValid, Some(s"cannot call private method '${ids.mkString(".")}'"))

            } else (matches, None)
          }

          ids match {
            case id :: Nil => {
              // println("case 1")
              val t = getFuncType(id, Left(symbolTable))
              // println(t)
              t
            }
            case _ => { 
              // println("case 2")
              val t = ids match {
                case instance :: method :: Nil => instance match {
                  case "this" => getParentClass(vars) match {
                    case Some(x) => ClassType(x.id)
                    case None => ErrorLogger.err(s"invalid call\n  - '${ids.last}' is not a function/method", func.pos)
                  }
                  case _ => getTypeFromVars(instance, vars, func.pos) 
                }
                case _ => {
                  // println("case 2.2")
                  getClassType(ids.init, func.pos, vars, true)
                }
              } 
              // println(ids)
              // println(t)
              t match {
                case x: ClassType => symbolTable.classes.get(x.class_id) match {
                  case Some(classTable) => {
                    // println(classTable)
                    val t = getFuncType(ids.last, Right(classTable))
                    // println(s"t => $t")
                    t
                  }
                  case _ => ???
                }
                case _ => ErrorLogger.err(s"invalid call\n  - '${ids.mkString(".")}' is not a function/method", func.pos)
              }
            }
          }
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
              val expType = getRValType(vars, head)

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
            val rType = getRValType(vars, exp)

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
              val rValType = getRValType(vars, exp)
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

    /* traverse a list of statements and error on semantic errors */
    def checkStatements(statements: List[Stat], vars: Table, isWhile: Boolean = false): Unit = {

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

      /* checks each statement */
      def checkStatement(statement: Stat): Unit = statement match {

        case x: Break => if (!isWhile) ErrorLogger.err("cannot use control-flow statment 'break' outside of while loop", x.pos)
        case x: Continue => if (!isWhile) ErrorLogger.err("cannot use control-flow statment 'continue' outside of while loop", x.pos)

        /* check declare statement */
        case Declare(t, id, rhs) => {
          val rType = getRValType(vars, rhs, (false, Some(t)))

          /* error if the left type is not the same as the right type */
          if (rType != t) ErrorLogger.err("invalid type for declare", rType, t, rhs.pos)

          /* check identifier hasn't already been declared, and add it to the scope */
          declareVar(id, t, vars, rhs.pos)
        }

        /* check assign statement */
        case AssignOrTypelessDeclare(x, y) => {/* get type of left and right hand sides of the assign */

          var rType: Type = NoType
          var lType: Type = NoType

          /* error if the identifier being reassigned is a function. */
          x match {
            case (ident@Ident(id)) => {
              if (symbolTable.contains(id) && !vars.contains(id)) {
                ErrorLogger.err("Cannot re-assign value for a function: " + id, ident.pos)
              } else if (isInferredTypeDefinition(x)) {
                rType = getRValType(vars, y, (true, None))
                lType = rType
                declareVar(id, rType, vars, y.pos)
              } else if (isTypelessParam(x)) {
                rType = getRValType(vars, y, (false, None))
                lType = rType
                vars.updateRecursive(id, Symbol(rType))
              } else {
                lType = getLValType(x, vars)
                rType = getRValType(vars, y, (false, Some(lType)))
              }
            }
            case _ => {
              lType = getLValType(x, vars)
              rType = getRValType(vars, y, (false, Some(lType)))
            }
          }

          /* error when attempting to assign an unknown type to another unknown type */
          if (lType == AnyType && rType == AnyType) ErrorLogger.err("invalid type for assign\n  cannot assign when both types are unknown", x.pos, y.pos)

          /* error when attempting to assign to a different type */
          if (lType != rType && rType != lType) ErrorLogger.err("invalid type for assign", rType, lType, x.pos, y.pos)
        }

        /* check read statement */
        case Read(x) => {
          val ltype = getLValType(x, vars)

          /* error if attempting to read to non int or char type. */
          if (ltype != IntType && ltype != CharType) ErrorLogger.err("invalid type for read", ltype, Seq(IntType, CharType), x.pos)
        }

        /* check free statement */
        case Free(x) => {
          val rType = getRValType(vars, x)

          /* error when freeing a non array or pair type. */
          rType match {
            case x: ArrayType =>
            case x: PairType =>
            case y => ErrorLogger.err("invalid type for free", y, Seq(ArrayType(AnyType), PairType(AnyType, AnyType)), x.pos)
          }
        }

        /* check return statement */
        case Return(x) => {
          val rType = getRValType(vars, x)

          /* error if we are not inside of a function */
          val funcTable = getFuncTable(vars, x)

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
          val rValType = getRValType(vars, x)
          if (rValType != IntType) ErrorLogger.err("invalid type for exit", rValType, IntType, x.pos)
        }

        /* check print statement */
        case Print(x) => getRValType(vars, x)

        /* check println statement */
        case Println(x) => getRValType(vars, x)

        /* check if statement */
        case If(p, xs, ys) => {

          /* error if condition not of boolean type */
          val rValType = getRValType(vars, p)
          if (rValType != BoolType) ErrorLogger.err("invalid type for if cond", rValType, BoolType, p.pos)

          /* check semantics of both branches of if statement */
          val thenVars = ChildTable(vars)
          checkStatements(xs, thenVars, isWhile)
          val elseVars = ChildTable(vars)
          checkStatements(ys, elseVars, isWhile)
          vars.addIf(thenVars, elseVars)
        }

        /* check while statement */
        case While(p, xs) => {

          /* error if while condition isn't of boolean type */
          val rtype = getRValType(vars, p)
          if (rtype != BoolType) ErrorLogger.err("invalid type for while cond", rtype, BoolType, p.pos)

          /* check semantics of loop body's statements */
          val child = ChildTable(vars)
          checkStatements(xs, child, true)
          vars.addWhile(child)
        }

        /* check begin statement, by checking the semantics of its body's statements */
        case Begin(xs) => {
          val child = ChildTable(vars)
          checkStatements(xs, child, isWhile)
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
      case Some(members) => c.funcs.foreach(func => {
        func.annotations.foreach(a => {
          if (!a.verify(func)) {
            errors += new TypeException(message = a.errorMsg, pos = Seq(func.pos))
          }
        })
        members.getMethodTable(func.fs._2) match {
          case Some(x) => checkStatements(func.stats, x, false)
          case None => {
            errors += new TypeException(message = s"invalid method declaration in '${c.class_id}'", pos = Seq(func.pos))
          }
        }
      }) 
      case None =>  
    })

    def trySetRightTypelessParam(rVal: RValue, vars: Table, rType: Type) = {
        rVal match {

          /* if its an identifier then check if it has a type in the parent and child scope maps yet*/
          case (y@Ident(id)) => vars.getType(id) match {
            case Some(x) => {
              // println(s"this is the right id: ${id}")
              // println(s"it should have the type NoType: ${x}")
              if (x == NoType) {
                vars.updateRecursive(id, Symbol(rType))
                // println(s"it should now be different: ${vars.getType(id)}")
                val tbl = getFuncTable(vars, y)
                tbl.paramIdTypes(id) = rType
              }
            }
            case None => 
          }
          case _ =>
        }
      }

    def checkParamRVal(rVal: RValue, vars: Table): Unit = {
        /* for an expression, match on the specific type of expression : */
          rVal match {
              /* for an array element with index : */
              case (elem@ArrayElem(id, exps)) => {
                  trySetRightTypelessParam(elem, vars, IntType)
              }

              /* for a unary operator, get its input and output types as val types */
              case UnaryOpExpr(op, exp) => {
                trySetRightTypelessParam(exp, vars, op.input)
              }

              /* for a binary operator : */
              case BinaryOpExpr(op, exp1, exp2) => {
                val types = op.input
                if (types.length == 1) {
                  trySetRightTypelessParam(exp1, vars, types(0))
                  trySetRightTypelessParam(exp2, vars, types(0))
                }
                trySetRightTypelessParam(exp1, vars, getRValType(vars, exp2))
                trySetRightTypelessParam(exp2, vars, getRValType(vars, exp1))
              }
              /* Default case - should be unreachable. */
              case _ => 
        }
      }

    def tryInferParam(statement: Stat, vars: Table): Unit = {

      statement match {

        case Declare(t, id, rhs) => {
          
          checkParamRVal(rhs, vars)
        }

        /* check assign statement */
        case AssignOrTypelessDeclare(x, y) => {/* get type of left and right hand sides of the assign */
          
          checkParamRVal(y, vars)
        }

        /* check return statement */
        case Return(x) => {
          trySetRightTypelessParam(x, vars, getFuncTable(vars, x).getReturnType)
          checkParamRVal(x, vars)
        }

        /* check exit statement */
        case Exit(x) => {
          trySetRightTypelessParam(x, vars, IntType)
          checkParamRVal(x, vars)
        }

        /* check if statement */
        case If(p, xs, ys) => {
            
          trySetRightTypelessParam(p, vars, BoolType)
            
          xs.foreach(stat => tryInferParam(stat, vars))
          ys.foreach(stat => tryInferParam(stat, vars))
          checkParamRVal(p, vars)
        }

        /* check while statement */
        case While(p, xs) => {

          trySetRightTypelessParam(p, vars, BoolType)
          xs.foreach(stat => tryInferParam(stat, vars))
          checkParamRVal(p, vars)
        }

        /* check begin statement, by checking the semantics of its body's statements */
        case Begin(xs) => {
          checkStatements(xs, vars)
        }

        case _ =>
      }
    }

    functions.foreach(func => {
      func.annotations.foreach(a => {
        if (!a.verify(func)) {
          errors += new TypeException(message = a.errorMsg, pos = Seq(func.pos))
        }
      })
      symbolTable.get(func.fs._2) match {
        case Some(x) => {
          checkStatements(func.stats, x, false)
          if (x.getReturnType == NoType) {
            errors += new TypeException(message = s"function's type is not inferrable", pos = Seq(func.pos))
          }
        }
        case None => errors += new TypeException(message = "invalid function declaration", pos = Seq(func.pos))
      }
    })

    checkStatements(statements, vars, false)

    return errors
  }

}
