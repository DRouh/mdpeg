package com.mdpeg

import org.parboiled2._
trait MultilineTablesParser extends PrimitiveRules {
  this: Parser =>

  def multiTable = ???

  def tableHeadRaw: Rule0 = {
    def headContentLine = rule(atomic(!tableHeadWidthSeparator ~ anyLine | blankLine))
    rule(tableBorder ~ headContentLine.+ ~ tableHeadWidthSeparator)
  }

  def tableBodyRaw: Rule0 = {
    def bodyContentLine = rule(atomic(!tableBorder ~ anyLine | blankLine))
    rule(tableHeadWidthSeparator ~ bodyContentLine.+ ~ tableBorder)
  }

  // ToDO in case of 1 column it can't be distinguished from tableBorder rule, so no !tableBorder applied here yet
  def tableHeadWidthSeparator: Rule0 = rule(atomic(!horizontalRule ~ (dashes ~ sp.*).+ ~ nl.?))
  def tableBorder: Rule0 = rule(atomic(!horizontalRule ~ dashes ~ nl))
  def tableCaption: Rule0 = rule(atomic("Table: " ~ anyChar.+ ~ nl.?))

  //aux rules
  private def dashes: Rule0 = rule((3 to 150).times("-"))
/* ToDo think about
* 1. capturing relative width of columns
* 2. capturing table caption
* 3. would it be easier to process row by row instead of immediately trying to split everything into cells?
* 4. rows are separated by blank lines
*/
}
