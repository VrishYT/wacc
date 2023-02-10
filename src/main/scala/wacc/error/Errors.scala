package wacc
package error

/* Holds constants used for error message generation */
package object error {
    /* No of lines before and after erroring line for "code snippet" */
    val NUM_LINES_AFTER = 1
    val NUM_LINES_BEFORE = 1
}

object Errors {

  private def combineOrUnknown(info: Seq[String], lines: Seq[String]): Seq[String] = {
    if (info.isEmpty) "unknown parse error" +: lines
    else info ++: lines
  }
  
  private def combineAsList(elems: List[String]): Option[String] = {
    val temp = elems.sorted.reverse match {
      case Nil => None
      case List(alt) => Some(alt)
      case List(alt1, alt2) => Some(s"$alt2 or $alt1")
      // If the result would contains "," then it's probably nicer to preserve any potential grouping using ";"
      case any@(alt::alts) if any.exists(_.contains(",")) => Some(s"${alts.reverse.mkString(", ")}; or $alt")
      case alt::alts => Some(s"${alts.reverse.mkString(", ")}, or $alt")
    }
    val res = temp match {
      case Some(x) => Some("expected " + temp.get)
      case None => None
    }
    res
  }

  case class WACCError(posSeq: Seq[(Int, Int)], source: Option[String], error_lines: WACCErrorLines) {

    private val pos = posSeq(0)

    private val printPos: String = s"(line ${Integer.toUnsignedString(pos._1)}, column ${Integer.toUnsignedString(pos._2)})"

    private val printLines: Seq[String] = error_lines.toSeqString(posSeq)

    override def toString(): String = s"\n${source.fold("")(name => s"In $name ")}$printPos:\n${printLines.mkString("  ", "\n  ", "")}"

  }

  object WACCError {
    def apply(pos: (Int, Int), source: Option[String], error_lines: WACCErrorLines) = new WACCError(Seq(pos), source, error_lines)
  }

  sealed trait WACCErrorLines {
    def toSeqString(pos: Seq[(Int, Int)]): Seq[String]
  }

  case class VanillaError(unexpected: Option[WACCErrorItem], expected: Seq[WACCErrorItem], reasons: Seq[String], lines: WACCErrorInfo) extends WACCErrorLines {
    override def toSeqString(pos: Seq[(Int, Int)]): Seq[String] = {
      val unexpected_ = unexpected.map("unexpected " + _.value())

      val expected_ = combineAsList(expected.map(_.value()).toList)

      val reasons_ = reasons.collect {
        case reason if reason.nonEmpty => Some("- " + reason)
      }

      combineOrUnknown((unexpected_ +: expected_ +: reasons_).flatten, lines.toSeqString(pos))
    }
  }

  case class SpecialisedError(msgs: Seq[String], lines: WACCErrorInfo) extends WACCErrorLines {
    override def toSeqString(pos: Seq[(Int, Int)]): Seq[String] = combineOrUnknown(msgs, lines.toSeqString(pos))
  }

  case class SemanticError(msg: String, lines: WACCErrorInfo) extends WACCErrorLines {
    override def toSeqString(pos: Seq[(Int, Int)]): Seq[String] = combineOrUnknown(Seq(msg), lines.toSeqString(pos))
  }

  sealed trait WACCErrorInfo {
    def toSeqString(pos: Seq[(Int, Int)]): Seq[String]

    def errorLineStart = " | "
    def errorPointer(caretAt: Int, caretWidth: Int) = s"${(" " * caretAt)}${"^" * caretWidth}"
  }
  
  case class ParseErrorInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int, numLinesBefore: Int, numLinesAfter: Int) extends WACCErrorInfo {
    override def toSeqString(posSeq: Seq[(Int, Int)]): Seq[String] = {
      val pos = posSeq(0)
      val maxLength: Int = pos._1.toString.length + 1

      Seq(s"") ++:
      linesBefore.zipWithIndex.map(line => s"%${maxLength}d$errorLineStart${line._1}".format(pos._1 - error.NUM_LINES_BEFORE + {line._2})) ++:
      Seq(s"%${maxLength}d$errorLineStart$line".format(pos._1), s"${" " * maxLength}$errorLineStart${errorPointer(errorPointsAt, errorWidth)}") ++:
      linesAfter.zipWithIndex.map(line => s"%${maxLength}d$errorLineStart${line._1}".format(pos._1 + line._2 + 1))
    }
  }

  case class TypecheckErrorInfo(lines: Seq[String]) extends WACCErrorInfo {

    def findWidth(line: String, start: Int): Int = {
      val sub = line.substring(start - 1)
      val width = sub.indexWhere(Seq(' ', '[', ']', '(', ')', '{', '}', ';', '\n', '\r', ',', '\t') contains _) match {
        case x if x == -1 => sub.length
        case x => x.max(1)
      }
      return width
    }

    override def toSeqString(posSeq: Seq[(Int, Int)]): Seq[String] = { 
      val pos = posSeq(0)
      val maxLength: Int = pos._1.toString.length + 1
      var previousPos = 0

      lines.zipWithIndex.map(line => line._2 match {
          case i if i < error.NUM_LINES_BEFORE => s"%${maxLength}d$errorLineStart${line._1}".format(pos._1 - error.NUM_LINES_BEFORE + {line._2})
          case i if i == error.NUM_LINES_BEFORE => {
            s"%${maxLength}d$errorLineStart${line._1}".format(pos._1) +
            "\n" +
            s"${" " * (maxLength + 2)}$errorLineStart" +
            posSeq.map(pos => {
              val errorPointsAt = pos._2 - previousPos
              val errorWidth = findWidth(line._1, pos._2)
              previousPos = errorPointsAt + errorWidth - 1
              s"${errorPointer((errorPointsAt - 1), errorWidth)}"
            }).mkString("")
          }
          case i => s"%${maxLength}d$errorLineStart${line._1}".format(pos._1 + i - 1)
      })
    }
  }

  sealed trait WACCErrorItem {
    def value(): String
  }
  case class WACCRaw(item: String) extends WACCErrorItem {
    override def value(): String = s"\"$item\""
  }
  case class WACCNamed(item: String) extends WACCErrorItem {
    override def value(): String = item
  }
  case object WACCEndOfInput extends WACCErrorItem {
    override def value(): String = "end of input"
  }
}

object ErrorLogger {

    import AST._

    /* Overloaded err method used for non-semantic errors. */
    def err(msg: String, exit: Int) = {
        System.err.println(msg)
        sys.exit(exit)
    }

    /* Overloaded err method used for semantic errors. Can have varying inputs depending on error found. */
    def err(msg: String, pos: (Int, Int)*) = throw new TypeException(msg, None, pos)
    def err(msg: String, actual: Type, expected: Type, pos: (Int, Int)*) = throw new TypeException(msg, Some(actual, Seq(expected)), pos)
    def err(msg: String, actual: Type, expected: Seq[Type], pos: (Int, Int)*) = throw new TypeException(msg, Some(actual, expected), pos)

}