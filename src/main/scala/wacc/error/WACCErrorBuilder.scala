package wacc
package error

import parsley.errors._
import Errors._


abstract class WACCErrorBuilder extends ErrorBuilder[WACCError] {
  
  /* formats the error information into a WACCError */
  override def format(pos: Position, source: Source, lines: ErrorInfoLines): WACCError = {
  WACCError(pos, source, lines)
  }

  /* the position of the error */
  type Position = (Int, Int)
  override def pos(line: Int, col: Int): Position = (line, col)

  /* the source file parsed */
  type Source = Option[String]
  override def source(sourceName: Option[String]): Source = sourceName.map(name => s"file '$name'")

  /* the lines detailing the error found */
  type ErrorInfoLines = WACCErrorLines

  /* define specialised and vanilla errors */
  override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine,
                            reasons: Messages, line: LineInfo): ErrorInfoLines = 
                            VanillaError(unexpected, expected, reasons, line)
  override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = 
                            SpecialisedError(msgs, line)

  /* items that could have fixed the syntax error */
  type ExpectedItems = Seq[WACCErrorItem]
  override def combineExpectedItems(alts: Set[Item]): ExpectedItems = {
    alts.toList
  }

  /* error messages */
  type Messages = Seq[Message]
  override def combineMessages(alts: Seq[Message]): Messages = alts.filter(_.nonEmpty)

  /* error found in code */
  type UnexpectedLine = Option[WACCErrorItem]
  override def unexpected(item: Option[Item]): UnexpectedLine = item

  /* line detailing how to fix the error */
  type ExpectedLine = Seq[WACCErrorItem]
  override def expected(alts: ExpectedItems): ExpectedLine = alts

  /* error message */  
  type Message = String
  override def reason(reason: String): Message = reason
  override def message(msg: String): Message = msg

  /* number of lines before erroneous line in code output */
  override val numLinesBefore: Int = error.NUM_LINES_BEFORE

  /* number of lines after erroneous line in code output */
  override val numLinesAfter: Int = error.NUM_LINES_AFTER

  /* code output for the error message */
  type LineInfo = ParseErrorInfo
  override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String],
                        errorPointsAt: Int, errorWidth: Int): LineInfo =
    ParseErrorInfo(line, linesBefore, linesAfter, errorPointsAt, errorWidth, this.numLinesBefore,
                   this.numLinesAfter)

  /* error items */  
  type Item = WACCErrorItem

  /* raw error items */
  type Raw = WACCRaw

  /* named errors */
  type Named = WACCNamed

  /* end of input error object */
  type EndOfInput = WACCEndOfInput.type

  /* error item functions */
  override def raw(item: String): Raw = WACCRaw(item)
  override def named(item: String): Named = WACCNamed(item)
  override val endOfInput: EndOfInput = WACCEndOfInput
}
