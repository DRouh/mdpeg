package com.mdpeg

import org.parboiled2._

import scala.collection.immutable
import scala.collection.immutable.::
import scala.util.Success
trait MultilineTablesParser extends PrimitiveRules {
  this: Parser =>

  def multiTable = rule { tableHeadRaw ~ tableBodyRaw ~ tableBorder ~ tableCaption.? }

  def tableHeadRaw: Rule1[Vector[String]] = {
    def headContentLine = rule(capture(atomic(!tableHeadWidthSeparator ~ anyLine | blankLine)))
    def contents :Rule1[Seq[String]] = rule(headContentLine.+)
    rule(tableBorder ~ capture(contents) ~ &(tableHeadWidthSeparator) ~> ((y:Seq[String],_:Any) => y.toVector))
  }

  def tableBodyRaw: Rule1[Vector[List[String]]] = {
    def bodyContentLine = rule(capture(atomic(!tableBorder ~ anyLine | blankLine)))
    def contents = rule(bodyContentLine.+)
    rule(capture(tableHeadWidthSeparator) ~ capture(contents) ~>
      ((sep:String, contents: Seq[String], _: Any) => parseBodyContent(sep, contents)))
  }

  // ToDO in case of 1 column it can't be distinguished from tableBorder rule, so no !tableBorder applied here yet
  def tableHeadWidthSeparator: Rule0 = rule(atomic(!horizontalRule ~ (dashes ~ sp.*).+ ~ nl.?))
  def tableBorder: Rule0 = rule(atomic(!horizontalRule ~ dashes ~ nl))
  def tableCaption: Rule0 = rule(atomic("Table: " ~ anyChar.+ ~ nl.?))

  //aux rules
  private def dashes: Rule0 = rule((3 to 150).times("-"))

  //aux functions

  /**
    * Splits body content into cells using 'width separator'
    * @param sep width separator string, '-- --- --'
    * @param contents raw contents of the table to be split into cells; blank lines are split points
    * @return vector of collumns containing cells.
    */
  private def parseBodyContent(sep:String, contents: Seq[String]): Vector[List[String]] = {
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
    val cells: Vector[List[String]] = rows
      .map(_.map(_.zip(widths).toList))
      .map(_.foldLeft(List.empty[List[String]]) {
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
      })
      .transpose(_.transpose.map(_.reverse.reduce(_ + "\r\n" + _)).toVector)
      .toVector
    cells
  }

  // ToDo investigate hot to re-use parser on differen inputs in order to avoid creation of a new parser on every line
  // needed to facilitate some internal processing
  private class PrimitvePaserHelper(val input:ParserInput) extends Parser with PrimitiveRules
}
