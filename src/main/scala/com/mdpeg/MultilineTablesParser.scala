package com.mdpeg

import org.parboiled2._

import scala.collection.immutable
import scala.collection.immutable.::
import scala.util.Success
trait MultilineTablesParser extends PrimitiveRules {
  this: Parser =>
  /*_*/
  def multiTable = rule(
    tableHeadRaw.? ~ tableBodyRaw ~ tableBorder ~ tableCaption.? ~>
      ((head: Option[Vector[String]], body:(Vector[List[String]], String), caption:Option[String]) => constructTable(head, body, caption))
  )
  /*_*/

  def constructTable(head: Option[Vector[String]], bodyWithWidth:(Vector[List[String]], String), caption:Option[String]) = {
    def calculateRelativeWidths(width: String) : Vector[Float] = {
      // todo improve calculation precision, use Largest Remainder Method and imrove upon relative error
      val columnWidths = width.split(' ').filter(_!="").map(_.length)
      val sum = columnWidths.sum.toFloat
      columnWidths.map(100*_.toFloat/sum).toVector
    }
    val (body, width: String) = bodyWithWidth
    val parsedHead = head.map(parseHeadContent(width, _))
    
    val relativeWidths = calculateRelativeWidths(width)
    val tableCaption = caption.map(y => MultilineTableCaption(Markdown(y)))
    val headRow: Option[MultilineTableRow] = parsedHead.map(_.map(inline => MultilineTableCell(Markdown(inline))).toVector)
    val bodyColumns: Vector[MultilineTableColumn] = body.map(_.map(inline => MultilineTableCell(Markdown(inline))).toVector)
    MultilineTableBlock(relativeWidths, tableCaption, headRow, bodyColumns)
  }

  def tableHeadRaw: Rule1[Vector[String]] = {
    def headContentLine = rule(capture(atomic(!tableHeadWidthSeparator ~ anyLine | blankLine)))
    def contents :Rule1[Seq[String]] = rule(headContentLine.+)
    rule(tableBorder ~ capture(contents) ~ &(tableHeadWidthSeparator) ~> ((headContent:Seq[String],_:Any) => headContent.toVector))
  }

  def tableBodyRaw: Rule1[(Vector[List[String]], String)] = {
    def bodyContentLine = rule(capture(atomic(!tableBorder ~ anyLine | blankLine)))
    def contents = rule(bodyContentLine.+)
    rule(capture(tableHeadWidthSeparator) ~ capture(contents) ~>
      ((sep:String, contents: Seq[String], _: Any) => parseBodyContent(sep, contents)))
  }

  // ToDO in case of 1 column it can't be distinguished from tableBorder rule, so no !tableBorder applied here yet
  def tableHeadWidthSeparator: Rule0 = rule(atomic(!horizontalRule ~ (dashes ~ sp.*).+ ~ nl.?))
  def tableBorder: Rule0 = rule(atomic(!horizontalRule ~ dashes ~ nl))
  def tableCaption: Rule1[String] = rule(atomic("Table: " ~ capture(anyChar.+ ~ nl.?)))

  //aux rules
  private def dashes: Rule0 = rule((3 to 150).times("-"))

  //aux functions
  private def parseHeadContent(sep: String, contents: Seq[String]) = {
    def isEmptyString(input: String) = {
      new PrimitvePaserHelper(input).blankLine.run() match {
        case Success(_) => true
        case _ => false
      }
    }

    val widths = sep.replaceAll("\r", "").replace("\n", "")
    val rows = contents.filter(!isEmptyString(_))
    val cells = rows.
      map(_.zip(widths).toList).
      map(_.
        foldLeft(List.empty[List[String]]) {
          case (acc, currentLine) =>
            (acc, currentLine) match {
              case (x :: xs, (c, '-')) => List(x.mkString + c.toString) :: xs
              case (xs, (_, ' ')) => List.empty[String] :: xs
              case (Nil, (c, '-')) => List(c.toString) :: Nil
              case _ => Nil
            }
        }.
        filter(_.nonEmpty).
        map(_.map(_.trim))).
    transpose(_.flatten).
    reverse.
    map(_.reduce(_+"\r\n"+_))

    cells
  }

  /**
    * Splits body content into cells using 'width separator'
    * @param sep width separator string, '-- --- --'
    * @param contents raw contents of the table to be split into cells; blank lines are split points
    * @return vector of collumns containing cells.
    */
  private def parseBodyContent(sep:String, contents: Seq[String]): (Vector[List[String]], String) = {
    def isEmptyString(input: String) = {
      new PrimitvePaserHelper(input).blankLine.run() match {
        case Success(_) => true
        case _ => false
      }
    }
    //'width separator', that is '---- -- --' string
    val widths = sep.replaceAll("\r", "").replace("\n", "")

    //split into rows by blank lines
    val rows = contents.foldLeft(List.empty[List[String]]) {
      case (acc, currentLine) =>
        val isEmptyLine = isEmptyString(currentLine)
        val cl = currentLine.replaceAll("\r", "").replace("\n", "")
        acc match {
          case x :: _ if isEmptyLine && x.isEmpty => acc
          case _ :: _ if isEmptyLine => List.empty[String] :: acc
          case x :: xs => (x :+ cl) :: xs
          case Nil => List(cl) :: Nil
        }
    }.reverse

    //split rows into cells by zipping every row with a 'width separator' and split upon every space it thereof
    val cells: Vector[List[String]] = rows.
      map(_.map(_.zip(widths).toList)).
      map(_.foldLeft(List.empty[List[String]]) {
        case (acc, currentLine) =>
          currentLine.foldLeft(List.empty[String]) {
            case (acc1, cl) =>
              (acc1, cl) match {
                case (x :: xs, (c, '-')) => (x + c.toString) :: xs
                case (x :: xs, (_, ' ')) => "" :: x.trim :: xs
                case (Nil, (c, '-')) => c.toString :: Nil
                case _ => Nil
              }
          }.filter(_ != "").reverse :: acc
      }).
      transpose(_.transpose.map(_.reverse.reduce(_ + "\r\n" + _)).toVector).
      toVector
    (cells, widths)
  }

  // ToDo investigate hot to re-use parser on differen inputs in order to avoid creation of a new parser on every line
  // needed to facilitate some internal processing
  private class PrimitvePaserHelper(val input:ParserInput) extends Parser with PrimitiveRules
}
