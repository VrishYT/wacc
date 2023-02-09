package wacc.error

object Errors {
  private def combineOrUnknown(info: Seq[String], lines: Seq[String]): Seq[String] = {
    if (info.isEmpty) WACCErrorBuilder.Unknown +: lines
    else info ++: lines
  }
  
  private def combineAsList(elems: List[String]): Option[String] = {
    val res = elems.sorted.reverse match {
      case Nil => None
      case List(alt) => Some(alt)
      case List(alt1, alt2) => Some(s"$alt2 or $alt1")
      // If the result would contains "," then it's probably nicer to preserve any potential grouping using ";"
      case any@(alt::alts) if any.exists(_.contains(",")) => Some(s"${alts.reverse.mkString(", ")}; or $alt")
      case alt::alts => Some(s"${alts.reverse.mkString(", ")}, or $alt")
    }
    Some("expected " + res.getOrElse(""))
  }

  case class WACCError(pos: (Int, Int), source: Option[String], error_lines: WACCErrorLines) {

    private val printPos: String = s"(line ${Integer.toUnsignedString(pos._1)}, column ${Integer.toUnsignedString(pos._2)})"

    private val printLines: Seq[String] = error_lines.toSeqString()

    override def toString(): String = s"\n Syntax error ${source.fold("")(name => s"in $name ")}$printPos:\n${printLines.mkString("  ", "\n  ", "")}"

  }

  sealed trait WACCErrorLines {
    def toSeqString(): Seq[String]
  }

  case class VanillaError(unexpected: Option[WACCErrorItem], expected: Seq[WACCErrorItem], reasons: Seq[String], lines: Seq[String]) extends WACCErrorLines {
    override def toSeqString(): Seq[String] = {
      val unexpected_ = unexpected.map("unexpected " + _.value())

      val expected_ = combineAsList(expected.map(_.value()).toList)

      val reasons_ = reasons.collect {
        case reason if reason.nonEmpty => Some("- " + reason)
      }
      combineOrUnknown((unexpected_ +: expected_ +: reasons_).flatten, lines)
    }
  }

  case class SpecialisedError(msgs: Seq[String], lines: Seq[String]) extends WACCErrorLines {
   override def toSeqString(): Seq[String] = combineOrUnknown(msgs, lines)
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
