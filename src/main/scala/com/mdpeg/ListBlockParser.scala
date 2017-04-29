package com.mdpeg

import org.parboiled2.{CharPredicate, Rule0, Rule1}

trait ListBlockParser extends PrimitiveRules {
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
  def bulletListItem: Rule1[String] = {
    def listStart = rule(!horizontalRule ~ bullet)
    def listRest  = rule(listContinuationBlock.*)
    rule(listStart ~ capture(listBlock) ~ listRest ~> (identity(_:String)))
  }

  // ordered list
  def orderedListTight: Rule1[OrderedList] = rule((orderedListItem.+ ~> (toOrderedList(_))) ~ blankLine.* ~ !orderedListSparse)
  def orderedListSparse: Rule1[OrderedList] = rule((orderedListItem ~ blankLine.*).+ ~> (toOrderedList(_)))
  def orderedListItem: Rule1[String] = {
    def listStart = rule(enumerator)
    def listRest  = rule(listContinuationBlock.*)
    rule(listStart ~ capture(listBlock) ~ listRest ~> (identity(_:String)))
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
  def listContinuationBlock: Rule0 = rule(blankLine.+ ~ (indent ~ listBlock).+)

  private def toUnorderedList(x: Seq[String]) = UnorderedList(Vector(x.map(Markdown).toVector))
  private def toOrderedList(x: Seq[String]) = OrderedList(Vector(x.map(Markdown).toVector))
}