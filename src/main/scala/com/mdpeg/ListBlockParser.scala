package com.mdpeg

import org.parboiled2._

trait ListBlockParser extends PrimitiveRules {
  this: Parser =>
  import CharPredicate._

  def list: Rule1[Block]                  = rule { unorderedList | orderedList }
  def unorderedList: Rule1[UnorderedList] = rule { bulletListTight | bulletListSparse }
  def orderedList: Rule1[OrderedList]     = rule { orderedListTight | orderedListSparse }

  // aux rules
  def bullet: Rule0 = rule { nonIndentSpace ~ anyOf("+-*") ~ sp.+ }
  def enumerator: Rule0 = rule { nonIndentSpace ~ Digit.+ ~ "." ~ sp.+ }

  // unordered list
  def bulletListTight: Rule1[UnorderedList] = rule((bulletListItem.+ ~> (toUnorderedList(_))) ~ blankLine.* ~ !bulletListSparse)
  def bulletListSparse: Rule1[UnorderedList] = rule((bulletListItem ~ blankLine.*).+ ~> (toUnorderedList(_)))
  def bulletListItem: Rule1[Vector[String]] = {
    def listStart = rule(!horizontalRule ~ bullet)
    def listRest  = rule(listContinuationBlock.* ~> ((x:Any)=> x.asInstanceOf[Vector[String]]))
    def ff(x:String, y: Vector[String]): Vector[String] = Vector(x) ++ y
    rule(listStart ~ capture(listBlock) ~ listRest ~> ((x:String, y: Vector[String]) => ff(x,y)))
  }

  // ordered list
  def orderedListTight: Rule1[OrderedList] = rule((orderedListItem.+ ~> (toOrderedList(_))) ~ blankLine.* ~ !orderedListSparse)
  def orderedListSparse: Rule1[OrderedList] = rule((orderedListItem ~ blankLine.*).+ ~> (toOrderedList(_)))
  def orderedListItem: Rule1[Vector[String]] = {
    def listStart = rule(enumerator)
    def listRest = rule(listContinuationBlock.* ~> ((x:Any)=> x.asInstanceOf[Vector[String]]))
    def ff(x:String, y: Vector[String]): Vector[String] = Vector(x) ++ y
    rule(listStart ~ capture(listBlock) ~ listRest ~> ((x:String, y: Vector[String]) => ff(x,y)))
  }

  // aux list rules
  def listBlock: Rule0 = {
    def blockContents = rule(anyLine)
    def notOptionallyIndentedAnyListItem = rule(!indent.? ~ (!bulletListItem | !orderedListItem))
    def notPossibleStartOfAnyList = rule(!indent ~ (!bullet | !enumerator))
    def blockRest = rule((notOptionallyIndentedAnyListItem ~ !blankLine ~ notPossibleStartOfAnyList ~ indentedLine.?).*)
    rule(blockContents ~ blockRest)
  }
  // ToDo improve continuation to handle inner lists
  def listContinuationBlock:Rule1[String] = {
    def blankLines:Rule1[String] = rule(capture(blankLine.+) ~> ((x:String) => identity(x)))
    def markdownSeparator:Rule1[String] = rule{MATCH ~ push("{{md-break}}") ~> ((x:String) => identity(x))}
    rule { (blankLines | markdownSeparator) ~ (indent ~ listBlock).+ }
  }

  private def toUnorderedList(x: Seq[Vector[String]]) = UnorderedList(x.map(_.map(Markdown)).toVector)
  private def toOrderedList(x: Seq[Vector[String]]) = OrderedList(x.map(_.map(Markdown)).toVector)
}