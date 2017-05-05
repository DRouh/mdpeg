package com.mdpeg

import org.parboiled2._
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
    def parseBodyContent(x: Vector[String]) = {
      println("parseBodyContent-------")
      println(s"x:${x}")
      x
    }

    rule(capture(tableHeadWidthSeparator) ~ capture(contents) ~> ((sep:String, contents: Seq[String], _: Any) => parseBodyContent(contents.toVector)))
  }

  // ToDO in case of 1 column it can't be distinguished from tableBorder rule, so no !tableBorder applied here yet
  def tableHeadWidthSeparator: Rule0 = rule(atomic(!horizontalRule ~ (dashes ~ sp.*).+ ~ nl.?))
  def tableBorder: Rule0 = rule(atomic(!horizontalRule ~ dashes ~ nl))
  def tableCaption: Rule0 = rule(atomic("Table: " ~ anyChar.+ ~ nl.?))

  //aux rules
  private def dashes: Rule0 = rule((3 to 150).times("-"))

  /* ToDo think about
* 1. capturing relative width of columns
* 2. rows are separated by blank lines
*/
}
