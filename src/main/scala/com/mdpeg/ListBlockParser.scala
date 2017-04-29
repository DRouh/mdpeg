package com.mdpeg

import org.parboiled2.{CharPredicate, Rule0, Rule1}

trait ListBlockParser extends PrimitiveRules {
  import CharPredicate._

  def list: Rule1[Block] = rule { unorderedList }
  def unorderedList: Rule1[UnorderedList] = ???
  def orderedList: Rule1[OrderedList] = ???

  // aux rules
  def bullet: Rule0     = rule { nonIndentSpace ~ anyOf("+-*") ~ sp.+ }
  def enumerator: Rule0 = rule { nonIndentSpace ~ Digit.+ ~ "." ~ sp.+ }

  def bulletListTight: Rule1[UnorderedList]  = rule((bulletListItem.+ ~> (toRawMd(_))) ~ blankLine.* ~ !bulletListSparse)
  def bulletListSparse: Rule1[UnorderedList] = rule((bulletListItem ~ blankLine.*).+ ~> (toRawMd(_)))
  def bulletListItem: Rule1[String] = {
    def listStart = rule(!horizontalRule ~ bullet)
    def listRest  = rule(listContinuationBlock.*)
    rule(listStart ~ capture(listBlock) ~ listRest ~> (identity(_:String)))
  }
  def listBlock: Rule0 = rule(anyLine ~ zeroOrMore(!indent.? ~ !bulletListItem ~ !blankLine ~ !indent ~ !bullet ~ indentedLine.?))
  def listContinuationBlock: Rule0 = rule(blankLine.+ ~ (indent ~ listBlock).+)

  //aux func
  def toRawMd(x: Seq[String]) = UnorderedList(Vector(x.map(Markdown).toVector))
}
