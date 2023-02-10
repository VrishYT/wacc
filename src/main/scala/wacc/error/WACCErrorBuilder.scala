package wacc
package error

import parsley.errors._

abstract class WACCErrorBuilder extends ErrorBuilder[WACCError] {
    override def format(pos: Position, source: Source, lines: ErrorInfoLines): WACCError = {
		// println(s"${source.fold("")(name => s"In $name ")}$pos:\n${lines.mkString("  ", "\n  ", "")}")
		WACCError(pos, source, lines)
    }

    type Position = (Int, Int)
    override def pos(line: Int, col: Int): Position = (line, col)

    type Source = Option[String]
    override def source(sourceName: Option[String]): Source = sourceName.map(name => s"file '$name'")

    type ErrorInfoLines = WACCErrorLines
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = VanillaError(unexpected, expected, reasons, line)
    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = SpecialisedError(msgs, line)

    type ExpectedItems = Seq[WACCErrorItem]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = {
      alts.toList
    }

    type Messages = Seq[Message]
    override def combineMessages(alts: Seq[Message]): Messages = alts.filter(_.nonEmpty)

    type UnexpectedLine = Option[WACCErrorItem]
    override def unexpected(item: Option[Item]): UnexpectedLine = item
    type ExpectedLine = Seq[WACCErrorItem]
    override def expected(alts: ExpectedItems): ExpectedLine = alts

    type Message = String
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    override val numLinesBefore: Int = error.NUM_LINES_BEFORE
    override val numLinesAfter: Int = error.NUM_LINES_AFTER

    type LineInfo = ParseErrorInfo
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): LineInfo =
      ParseErrorInfo(line, linesBefore, linesAfter, errorPointsAt, errorWidth, this.numLinesBefore, this.numLinesAfter)

    type Item = WACCErrorItem
    type Raw = WACCRaw
    type Named = WACCNamed
    type EndOfInput = WACCEndOfInput.type
    override def raw(item: String): Raw = WACCRaw(item)
    override def named(item: String): Named = WACCNamed(item)
    override val endOfInput: EndOfInput = WACCEndOfInput
  
}

private object WACCErrorBuilder {
	val Unknown = "unknown parse error"
}
