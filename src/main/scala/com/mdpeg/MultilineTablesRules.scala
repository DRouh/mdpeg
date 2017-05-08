package com.mdpeg
import org.parboiled2._

import scala.collection.immutable.::
import scala.compat.Platform.EOL
import scala.util.Success

trait MultilineTablesRules {
  this: Parser with PrimitiveRules =>

  private def anyLineTable  : Rule0 = rule(!nl ~ !EOI ~ anyCharTable.* ~ (nl | ""))
  private def anyCharTable  : Rule0 = rule(anyChar | backTick)

  /*_*/
  def multiTable: Rule1[MultilineTableBlock] = rule(
    tableHeadRaw.? ~ tableBody ~ tableBorder ~ tableCaption.? ~ capture(nl.* | blankLine.*) ~>
      ((head: Option[Vector[String]], body: (Vector[List[String]], String), caption: Option[String], _: String) => constructTable(head, body, caption))
  )
  /*_*/

  def constructTable(head: Option[Vector[String]], bodyWithWidth: (Vector[List[String]], String), caption: Option[String]): MultilineTableBlock = {
    def calculateRelativeWidths(width: String): Vector[Float] = {
      // todo improve calculation precision, use Largest Remainder Method and improve upon relative error
      val columnWidths = width.split(' ').filter(_ != "").map(_.length)
      val sum = columnWidths.sum.toFloat
      columnWidths.map(100 * _.toFloat / sum).toVector
    }

    val (body, width: String) = bodyWithWidth
    val parsedHead = head.map(parseHeadContent(width, _))

    val relativeWidths = calculateRelativeWidths(width)
    val tableCaption = caption.map(inline => MultilineTableCaption(Markdown(inline)))
    val headRow: Option[MultilineTableRow] = parsedHead.map(_.map(inline => MultilineTableCell(Markdown(inline))).toVector)
    val bodyColumns: Vector[MultilineTableColumn] = body.map(_.map(inline => MultilineTableCell(Markdown(inline))).toVector)
    MultilineTableBlock(relativeWidths, tableCaption, headRow, bodyColumns)
  }

  def tableHeadRaw: Rule1[Vector[String]] = {
    def headContentLine = rule(capture(atomic(!tableHeadWidthSeparator ~ anyLineTable | blankLine)))
    def contents: Rule1[Seq[String]] = rule(headContentLine.+)
    rule(tableBorder ~ capture(contents) ~ &(tableHeadWidthSeparator) ~> ((headContent: Seq[String], _: Any) => headContent.toVector))
  }

  def tableBody: Rule1[(Vector[List[String]], String)] = {
    def bodyContentLine = rule(capture(atomic(!tableBorder ~ anyLineTable | blankLine)))
    def contents = rule(bodyContentLine.+)
    rule(capture(tableHeadWidthSeparator) ~ capture(contents) ~>
      ((sep: String, contents: Seq[String], _: Any) => parseBodyContent(sep, contents)))
  }

  // ToDO in case of 1 column it can't be distinguished from tableBorder rule, so no !tableBorder applied here yet
  def tableHeadWidthSeparator: Rule0 = rule(atomic(!horizontalRule ~ (dashes ~ sp.*).+ ~ nl.?))
  def tableBorder: Rule0 = rule(atomic(!horizontalRule ~ dashes ~ nl))
  def tableCaption: Rule1[String] = rule(atomic("Table: " ~ capture(anyCharTable.+) ~ (endLine | nl.?)))
  def dashes: Rule0 = rule((3 to 150).times("-"))

  /**
    * Splits header content into cells using 'width separator'
    * @param sep width separator string, '-- --- --'
    * @param contents raw contents of the table head to be split into cells; blank lines removed, that is,
    *                 there's no support for multi-row headers
    * @return vector of cells in a header row
    */
  private def parseHeadContent(sep: String, contents: Seq[String]) = {
    def isEmptyString(input: String) = {
      new PrimitvePaserHelper(input).blankLine.run() match {
        case Success(_) => true
        case _ => false
      }
    }

    val widths = sep.replaceAll("\r", "").replace("\n", "")
    val rows = contents.filter(!isEmptyString(_)).toList
    val indexes = " -".r.findAllMatchIn(widths).map(_.start + 1).toList
    val cells =
      transposeAnyShape(rows.map(s => listSplit(indexes, s))).
      map(_.reduce((s1,s2) => trimEndWithEnding(s1) + EOL + s2)).
      map(s => trimEndWithEnding(s))

    cells
  }

  /**
    * Splits body content into cells using 'width separator'
    * @param sep width separator string, '-- --- --'
    * @param contents raw contents of the table body to be split into cells; blank lines are split points
    * @return vector of columns containing cells.
    */
  private def parseBodyContent(sep: String, contents: Seq[String]): (Vector[List[String]], String) = {
    def isEmptyString(input: String) = {
      new PrimitvePaserHelper(input).blankLine.run() match {
        case Success(_) => true
        case _ => false
      }
    }

    //'width separator', that is '---- -- --' string
    val widths = sep.replaceAll("\r", "").replace("\n", "")

    //split into rows by blank lines
    val rows: List[List[String]] = contents.
      foldLeft(List.empty[List[String]]) {
        case (acc, currentLine) =>
          val isEmptyLine = isEmptyString(currentLine)
          val cl = currentLine.replaceAll("\r", "").replace("\n", "")
          acc match {
            case x :: _ if isEmptyLine && x.isEmpty => acc
            case _ :: _ if isEmptyLine => List.empty[String] :: acc
            case x :: xs => (x :+ cl) :: xs
            case Nil => List(cl) :: Nil
          }
      }.
      reverse.
      filter(_.nonEmpty)

    //split by every change from spaces to dashes in width separator
    val indexes = " -".r.findAllMatchIn(widths).map(_.start + 1).toList
    val cells: Vector[List[String]] =
      transposeAnyShape(rows.
          map(_.map(s => listSplit(indexes, s))).
          map(transposeAnyShape(_).
              map(strings => trimEnd(strings.reduce((s1, s2) => trimEnd(s1) + EOL + s2))).
              filter(_ != ""))
      ).toVector
    (cells, widths)
  }

  // ToDo investigate hot to re-use parser on different inputs in order to avoid creation of a new parser on every line
  // needed to facilitate some internal processing
  private class PrimitvePaserHelper(val input: ParserInput) extends Parser with PrimitiveRules
}
