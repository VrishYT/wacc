package wacc.error

object Errors {
  private def combineOrUnknown(info: Seq[String], lines: Seq[String]): Seq[String] = {
    if (info.isEmpty) WACCErrorBuilder.Unknown +: lines
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

  case class WACCError(pos: (Int, Int), source: Option[String], error_lines: WACCErrorLines) {

    private val printPos: String = s"(line ${Integer.toUnsignedString(pos._1)}, column ${Integer.toUnsignedString(pos._2)})"

    private val printLines: Seq[String] = error_lines.toSeqString(pos)

    override def toString(): String = s"\n Syntax error ${source.fold("")(name => s"in $name ")}$printPos:\n${printLines.mkString("  ", "\n  ", "")}"

  }

  sealed trait WACCErrorLines {
    def toSeqString(pos: (Int, Int)): Seq[String]
  }

  case class VanillaError(unexpected: Option[WACCErrorItem], expected: Seq[WACCErrorItem], reasons: Seq[String], lines: WACCErrorInfo) extends WACCErrorLines {
    override def toSeqString(pos: (Int, Int)): Seq[String] = {
      val unexpected_ = unexpected.map("unexpected " + _.value())

      val expected_ = combineAsList(expected.map(_.value()).toList)

      val reasons_ = reasons.collect {
        case reason if reason.nonEmpty => Some("- " + reason)
      }

      combineOrUnknown((unexpected_ +: expected_ +: reasons_).flatten, lines.toSeqString(pos))
    }
  }

  case class SpecialisedError(msgs: Seq[String], lines: WACCErrorInfo) extends WACCErrorLines {
    override def toSeqString(pos: (Int, Int)): Seq[String] = combineOrUnknown(msgs, lines.toSeqString(pos))
  }

  sealed trait WACCErrorInfo {
    def toSeqString(pos: (Int, Int)): Seq[String]
  }
  
  case class ParseErrorInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int, numLinesBefore: Int, numLinesAfter: Int) extends WACCErrorInfo {
    override def toSeqString(pos: (Int, Int)): Seq[String] = {
      val maxLength: Int = pos._1.toString.length + 1

      Seq(s"") ++:
      linesBefore.zipWithIndex.map(line => s"%${maxLength}d$errorLineStart${line._1}".format(pos._1 - numLinesBefore + {line._2})) ++:
      Seq(s"%${maxLength}d$errorLineStart$line".format(pos._1), s"${" " * maxLength}$errorLineStart${errorPointer(errorPointsAt, errorWidth)}") ++:
      linesAfter.zipWithIndex.map(line => s"%${maxLength}d$errorLineStart${line._1}".format(pos._1 + line._2 + 1))
    }

    private val errorLineStart = " | "
    private def errorPointer(caretAt: Int, caretWidth: Int) = s"${(" " * caretAt)}${"^" * caretWidth}"
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
