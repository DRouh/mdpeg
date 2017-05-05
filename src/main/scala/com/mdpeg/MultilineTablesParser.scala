package com.mdpeg

import org.parboiled2._

import scala.util.Success
trait MultilineTablesParser extends PrimitiveRules {
  this: Parser =>

  def multiTable = rule { tableHeadRaw ~ tableBodyRaw ~ tableBorder ~ tableCaption.? }

  def tableHeadRaw: Rule1[Vector[String]] = {
    def headContentLine = rule(capture(atomic(!tableHeadWidthSeparator ~ anyLine | blankLine)))
    def contents :Rule1[Seq[String]] = rule(headContentLine.+)
    rule(tableBorder ~ capture(contents) ~ &(tableHeadWidthSeparator) ~> ((y:Seq[String],_:Any) => y.toVector))
  }

  def tableBodyRaw: Rule1[Vector[String]] = {
    def bodyContentLine = rule(capture(atomic(!tableBorder ~ anyLine | blankLine)))
    def contents = rule(bodyContentLine.+)
    def parseBodyContent(sep:String, contents: Seq[String]) = {
      def isEmptyString(input:String) = {
        new PrimitvePaserHelper(input).blankLine.run() match {
          case Success(node) => true
          case _ => false
        }
      }

      // ToDo scan through to determine positions of every width separator
      val widths = sep.split(' ').filter(_ != "").map(_.replaceAll("\r","").replace("\n","")).toVector

      //ToDo filter out empty entries that might occur in case there're more than 2 empty lines in a row
      val rows = contents.foldLeft(List.empty[List[String]]) {
        case (acc, currentLine) =>
          if(isEmptyString(currentLine))
            List.empty[String] :: acc
          else
            acc match {
              case x::xs => (x :+ currentLine) :: xs
              case Nil => List(currentLine) :: Nil
            }
      }
      contents.toVector
    }

    rule(capture(tableHeadWidthSeparator) ~ capture(contents) ~> ((sep:String, contents: Seq[String], _: Any) => parseBodyContent(sep, contents)))
  }

  // ToDO in case of 1 column it can't be distinguished from tableBorder rule, so no !tableBorder applied here yet
  def tableHeadWidthSeparator: Rule0 = rule(atomic(!horizontalRule ~ (dashes ~ sp.*).+ ~ nl.?))
  def tableBorder: Rule0 = rule(atomic(!horizontalRule ~ dashes ~ nl))
  def tableCaption: Rule0 = rule(atomic("Table: " ~ anyChar.+ ~ nl.?))

  //aux rules
  private def dashes: Rule0 = rule((3 to 150).times("-"))



  // ToDo investigate hot to re-use parser on differen inputs in order to avoid creation of a new parser on every line
  // needed to facilitate some internal processing
  private class PrimitvePaserHelper(val input:ParserInput) extends Parser with PrimitiveRules

  /* ToDo think about
* 1. capturing relative width of columns
* 2. rows are separated by blank lines
*/
}
