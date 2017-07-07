package org.mdpeg.parsers

import org.mdpeg._
import org.mdpeg.ast._
import org.parboiled2._

import scala.collection.immutable.::
import scala.compat.Platform.EOL
import scala.util.Success

private[mdpeg] trait MultilineTablesRules {
  this: Parser with PrimitiveRules =>

  type WidthSeparator = String
  type RawBody = Vector[List[String]]

  def multiTable: Rule1[MultilineTableBlock] = {
    /*_*/
    rule(tableHeadRaw.? ~ tableBody ~ tableCaption ~>
      ((head:Option[Vector[String]], body: (Vector[List[String]], String), caption: Option[String]) =>
        constructTable(head, body, caption)))
    /*_*/
  }

  def tableHeadRaw: Rule1[Vector[String]] = {
    def headContentLine = rule(capture(!tableHeadWidthSeparator ~ (anyLineTable | blankLine)))
    rule(!horizontalRule ~ tableBorder ~ headContentLine.+ ~ &(tableHeadWidthSeparator) ~> ((headContent: Seq[String]) =>
      headContent.toVector))
  }

  def tableBody: Rule1[(RawBody, WidthSeparator)] = {
    def bodyContentLine:Rule1[String] = rule(capture(!tableBorder ~ (anyLineTable | blankLine)))
    rule(capture(tableHeadWidthSeparator) ~ bodyContentLine.+ ~> ((sep: String, contents: Seq[String]) =>
      parseBodyContent(sep, contents)))
  }

  def tableCaption: Rule1[Option[String]] = rule{
    !horizontalRule ~ tableBorder ~ "Table:" ~ sps ~ capture(anyCharTable.+) ~ nl.? ~ blankLine.* ~> ((s:String) => Some(s)) |
      tableBorder ~ capture(sps) ~ nl.? ~ blankLine.* ~> ((_:String) => None)
  }

  // ToDO in case of 1 column it can't be distinguished from tableBorder rule, so no !tableBorder applied here yet
  def tableHeadWidthSeparator: Rule0 = rule(!horizontalRule ~ (dashes ~ sps).+ ~ nl.?)

  def tableBorder: Rule0 = rule(dashes ~ nl)

  def dashes: Rule0 = rule((3 to 150).times("-"))

  private def anyLineTable: Rule0 = rule(anyCharTable.+ ~ nl)

  private def anyCharTable: Rule0 = rule(!nl ~ !EOI ~ ANY)

  //aux functions
  private def constructTable(head: Option[Vector[String]], bodyWithWidth: (RawBody, WidthSeparator), caption:
  Option[String]): MultilineTableBlock = {
    def calculateRelativeWidths(width: String): Vector[Float] = {
      // todo improve calculation precision, use Largest Remainder Method and improve upon relative error
      val columnWidths = width.split(' ').filter(_ != "").map(_.length)
      val sum = columnWidths.sum.toFloat
      columnWidths.map(100 * _.toFloat / sum).toVector
    }

    val (body, width: String) = bodyWithWidth
    val parsedHead = head.map(parseHeadContent(width, _))
    val relativeWidths = calculateRelativeWidths(width)

    val tableCaption: Option[MultilineTableCaption] = caption.
      map { inline =>
        val labelPattern = """\\label\{(.*)\}""".r
        val maybeLabel = (for (ex <- labelPattern.findAllMatchIn(inline)) yield (ex.group(0), ex.group(1))).toVector
          .headOption
        val maybeWholeMatch = maybeLabel.map(_._1).getOrElse("")
          MultilineTableCaption(Vector(Markdown(RawMarkdownContent(inline.replace(maybeWholeMatch, "")))), maybeLabel.map(_._2))
    }

    val headRow: Option[MultilineTableRow] = parsedHead.
      map(_.map(inline => MultilineTableCell(Vector(Markdown(RawMarkdownContent(inline))))).toVector)

    val bodyColumns: Vector[MultilineTableColumn] = body.
      map(_.map(inline => MultilineTableCell(Vector(Markdown(RawMarkdownContent(inline))))).toVector)

    MultilineTableBlock(relativeWidths, tableCaption, headRow, bodyColumns)
  }

  /**
    * Splits header content into cells using 'width separator'
    *
    * @param sep      width separator string, '-- --- --'
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

    val cells = rows.map(s => listSplit(indexes, s)) |>
      transposeAnyShape |> {x =>
        x.map(_.reduce(trimEndWithEnding(_) + EOL + _)).map(trimEndWithEnding)
      }

    cells
  }

  /**
    * Splits body content into cells using 'width separator'
    *
    * @param sep      width separator string, '-- --- --'
    * @param contents raw contents of the table body to be split into cells; blank lines are split points
    * @return vector of columns containing cells.
    */
  private def parseBodyContent(sep: String, contents: Seq[String]): (RawBody, WidthSeparator) = {
    def isEmptyString(input: String) = {
      new PrimitvePaserHelper(input).blankLine.run() match {
        case Success(_) => true
        case _ => false
      }
    }

    //'width separator', that is '---- -- --' string
    val widths = sep.replaceAll("\r", "").replace("\n", "")

    //split into rows by blank lines
    val rows: List[List[String]] = contents.foldLeft(List.empty[List[String]]) { case (acc, currentLine) =>
      val isEmptyLine = isEmptyString(currentLine)
      val cl = currentLine.replaceAll("\r", "").replace("\n", "")
      acc match {
        case x :: _ if isEmptyLine && x.isEmpty => acc
        case _ :: _ if isEmptyLine => List.empty[String] :: acc
        case x :: xs => (x :+ cl) :: xs
        case Nil => List(cl) :: Nil
      }
    }.reverse.filter(_.nonEmpty)

    //split by every change from spaces to dashes in width separator
    val indexes = " -".r.findAllMatchIn(widths).map(_.start + 1).toList
    val cells: Vector[List[String]] = rows.
      map(_.map(s => listSplit(indexes, s))).
      map(_ |> transposeAnyShape |> (_.map(_.reduce(trimEnd(_) + EOL + _) |> trimEnd))) |>
      transposeAnyShape |>
      (_.toVector)

    (cells, widths)
  }

  // ToDo investigate hot to re-use parser on different inputs in order to avoid creation of a new parser on every line
  // needed to facilitate some internal processing
  private class PrimitvePaserHelper(val input: ParserInput) extends Parser with PrimitiveRules

}
