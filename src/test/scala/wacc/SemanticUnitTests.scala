package wacc


import scala.io.Source
import scala.jdk.CollectionConverters._

import org.scalatest.Assertions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Ignore
import org.scalatest.Tag

import java.nio.file.{Files, Paths, Path}

import AST._

class SemanticUnitTests extends AnyFunSuite {

    test("Char declaration + exit ord is valid") {
        val fs = List()
        val declare = Declare(CharType, "c", CharLiteral('x'))
        val ord = UnaryOpExpr(Ord, Ident("c"))
        val stats = List(declare, Exit(ord))
        val p = Program(fs, stats)

        SemanticChecker.typecheck(p)
    }
    
    test("Bool Declaration is valid") {
        val fs = List()
        val stats = List(Declare(BoolType, "b", BoolLiteral(false)))
        val p = Program(fs, stats)

        SemanticChecker.typecheck(p)
    }
    
    test("Bool Declaration with an int is invalid") {
        val fs = List()
        val stats = List(Declare(BoolType, "b", IntLiteral(1)))
        val p = Program(fs, stats)

        assertThrows[scala.NotImplementedError] {
            SemanticChecker.typecheck(p)
        }
    }
}