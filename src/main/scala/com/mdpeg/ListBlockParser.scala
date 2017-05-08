package com.mdpeg

import org.parboiled2._

trait ListBlockParser {
  this: Parser with PrimitiveRules =>
  import CharPredicate._

  def list: Rule1[Block]                  = rule { unorderedList | orderedList }
  def unorderedList: Rule1[UnorderedList] = rule { bulletListTight | bulletListSparse }
  def orderedList: Rule1[OrderedList]     = rule { orderedListTight | orderedListSparse }

  // aux rules
  def bullet: Rule0 = rule { nonIndentSpace ~ anyOf("+-*") ~ sp.+ }
  def enumerator: Rule0 = rule { nonIndentSpace ~ Digit.+ ~ "." ~ sp.+ }

  // unordered list
  def bulletListTight: Rule1[UnorderedList] = rule((bulletListItem.+ ~> toUnorderedList _) ~ blankLine.* ~ !bulletListSparse)
  def bulletListSparse: Rule1[UnorderedList] = rule((bulletListItem ~ blankLine.*).+ ~> (toUnorderedList(_)))
  def bulletListItem: Rule1[Vector[String]] = {
    def listStart = rule(!horizontalRule ~ bullet)
    def listRest  = rule(listContinuationBlock.*)
    def ff(x:String, y: String): Vector[String] = {
      //println(s"bulletListItem:Vector($x) ++ ($y)")
      Vector(x+y)
    }
    rule(listStart ~ capture(listBlock) ~ capture(listRest) ~> ((x:String, y: String) => ff(x,y)))
  }

  // ordered list
  def orderedListTight: Rule1[OrderedList] = rule((orderedListItem.+ ~> (toOrderedList(_))) ~ blankLine.* ~ !orderedListSparse)
  def orderedListSparse: Rule1[OrderedList] = rule((orderedListItem ~ blankLine.*).+ ~> (toOrderedList(_)))
  def orderedListItem: Rule1[Vector[String]] = {
    def listStart = rule(enumerator)
    def listRest  = rule(listContinuationBlock.*)
    def ff(x:String, y: String): Vector[String] = {
      //println(s"bulletListItem:Vector($x) ++ ($y)")
      Vector(x+y)
    }
    rule(listStart ~ capture(listBlock) ~ capture(listRest) ~> ((x:String, y: String) => ff(x,y)))
  }


  // aux list rules
  def listBlock: Rule0 = {
    def blockContents = rule(anyLine)
    def notOptionallyIndentedAnyListItem = rule(!(listIndent.? ~ (!bulletListItem | !orderedListItem)))
    def notPossibleStartOfAnyList        = rule(!listIndent ~ (!bullet | !enumerator))
    def blockRest = rule((notOptionallyIndentedAnyListItem ~ !blankLine ~ notPossibleStartOfAnyList ~ !indentedLine.?).*)
    rule(blockContents ~ blockRest)
  }

  // ToDo improve continuation to handle inner lists
  def listContinuationBlock: Rule0 = {
    def continuation: Rule0 = rule((listIndent ~ listBlock).+)
    rule((blankLine.+ | MATCH) ~ continuation)
  }

  // aux rules
  private def listIndent = rule(indent | halfIndent)
  private def halfIndent = rule("  ")

  // aux funcs
  private def toUnorderedList(x: Seq[Vector[String]]) = UnorderedList(x.flatten.map(Markdown).toVector)
  private def toOrderedList(x: Seq[Vector[String]]) = OrderedList(x.flatten.map(Markdown).toVector)
}