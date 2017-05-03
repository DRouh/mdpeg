package com.mdpeg

import org.parboiled2._
trait MultilineTablesParser extends PrimitiveRules {
  this: Parser =>
  import CharPredicate._

  def multiTable = ???

  def tableHead: Rule1[String] = rule(tableBorder ~ capture(anyChar.+) ~ nl ~ tableHeadWidthSeparator)
  // ToDO in case of 1 column it can't be distinguished from tableBorder rule, so no !tableBorder applied here yet
  def tableHeadWidthSeparator:Rule0 = rule(!horizontalRule ~ ((3 to 150).times("-") ~ sp.*).+ ~ nl.?)
  def tableBorder: Rule0 = rule(!horizontalRule ~ (3 to 150).times("-") ~ nl)
  def tableCaption: Rule0 = rule("Table: " ~ anyChar.+ ~ nl.?)
  //ToDo def heading content?
/* ToDo think about
* 1. capturing relative width of columns
* 2. capturing table caption
* 3. would it be easier to process row by row instead of immediately trying to split everything into cells?
* 4. rows are separated by blank lines
*/
}
