package wacc

import scala.io.Source

import parsley.{Parsley, Success, Failure}
import parsley.character.digit
import parsley.expr.chain
import parsley.implicits.character.charLift
import parsley.combinator._
import Lexing._
import Parser._


object Main {
    def main(args: Array[String]): Unit = {

        //val file = openFile(args.head)

        UNOP_MINUS.parse(args.head) match {
            case Success(x) => println(s"${args.head} = $x")
            case Failure(msg) => println(msg)
        }
    }

    def openFile(file1: String) : String = {
        val builder = new StringBuilder()
        for (line <- Source.fromFile(file1).getLines) {
            builder.append(line)
            builder.append(" ")
        }
        val result = builder.toString()
        return result
    }
}

/*
lazy val integer = digit.foldLeft1[BigInt](0)((n, d) => n * 10 + d.asDigit)

        val add = (x: BigInt, y: BigInt) => x + y
        val sub = (x: BigInt, y: BigInt) => x - y
        val mul = (x: BigInt, y: BigInt) => x * y
        val div = (x: BigInt, y: BigInt) => x / y

        lazy val expr: Parsley[BigInt] =
            chain.left1[BigInt](
                ('(' ~> expr <~ ')') <|> integer,
                ('+' #> add) <|> ('-' #> sub) <|> ('*' #> mul) <|> ('/' #> div)
            )

        expr.parse(args.head) match {
            case Success(x) => println(s"${args.head} = $x")
            case Failure(msg) => println(msg)
        }
*/