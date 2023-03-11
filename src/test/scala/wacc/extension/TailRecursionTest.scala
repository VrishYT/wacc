package wacc
package extension

import ast._
import front._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest._

class TailRecursiveTypecheckTest extends AnyFunSuite {

    test("Adding @tailrec flag does not break typechecker") {

        val program = Program(
            List(
                Func(
                    List(TailRecursiveAnnotation),
                    (IntType,"sumToFive"),
                    List(Param(IntType,"x")(0,0)),
                    List(
                        If(
                            BinaryOpExpr(Equal,Ident("x")(0,0),IntLiteral(5)(0,0))((0,0), (0,0)), // cond
                            List(Return(Ident("x")(0,0))), // then
                            List( // else
                                Declare(IntType, "y", Call(
                                        "sumToFive",
                                        List(BinaryOpExpr(Add,Ident("x")(0,0),IntLiteral(1)(0,0))((0,0), (0,0)))
                                    )(0,0)
                                ), 
                                Return(Ident("y")(0,0))
                            )
                        )
                    )
                )(0,0)
            ),
            List(
                Declare(IntType,"x",Call("sumToFive",List(IntLiteral(0)(0,0)))(0,0)), 
                Exit(Ident("x")(0,0))
            )
        )

        val symbolTable = new SymbolTable
        val errors = SemanticChecker.typecheck(program, symbolTable)

        assert(errors.isEmpty, errors.mkString("\n"))

    }

    test("TailRecursiveAnnnotation.process() correctly mutates statements for basic tail recursion") {

        // reflects simpleTailRecursion.wacc - function definition

        val recursive = List(
            If(
                BinaryOpExpr(Equal, Ident("x")(0, 0), IntLiteral(5)(0, 0))((0, 0), (0, 0)),
                List(
                    Return(Ident("x")(0, 0))
                ),
                List(
                    Declare(IntType, "y", Call("func", List(BinaryOpExpr(Add, Ident("x")(0, 0), IntLiteral(1)(0, 0))((0, 0), (0, 0))))(0, 0)),
                    Return(Ident("y")(0, 0))
                )
            )
        )

        val optimized = TailRecursiveAnnotation.process(recursive)

        val expected = List(
            While(
                BinaryOpExpr(NotEqual, Ident("x")(0, 0), IntLiteral(5)(0, 0))((0, 0), (0, 0)),
                List(
                    Assign(Ident("x")(0, 0), BinaryOpExpr(Add, Ident("x")(0, 0), IntLiteral(1)(0, 0))((0, 0), (0, 0)))
                )
            ),
            Return(Ident("x")(0, 0))
        )

        assert(optimized == expected)

    }

}