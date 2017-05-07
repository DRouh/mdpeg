package com.mdpeg

import org.parboiled2._

class BlockParser(val input: ParserInput) extends Parser with PrimitiveRules with ListBlockParser with MultilineTablesParser {
  def InputLine: Rule1[Seq[Block]] = rule(block.+ ~ EOI)

  def block: Rule1[Block] = rule { blockQuote | verbatim | heading | list | multiTable | horizontalRule | paragraph | plain  }

  //block definitions
  def verbatim : Rule1[Verbatim] = {
    def inlineCodeChar = rule( inline | mathChar | specialChar ) // ToDo add blank line?
    def verbatimBlockBound = rule(3.times("`"))
    def verbatimBlockContents = rule((nl | inlineCodeChar.+ ~ nl).+)
    rule (verbatimBlockBound ~ capture(verbatimBlockContents) ~ verbatimBlockBound ~ blankLine.* ~> Verbatim)
  }

  def blockQuote : Rule1[BlockQuote] = { // ToDo think if should store a Markdown instead of concatenated strings
    def blockQuoteLine: Rule1[String] = rule(nonIndentSpace ~ ">" ~ sp.+ ~ capture(anyLine))
    // ToDo think if keep line breaks or keep as it is (i.e. replaced with spaces)
    def toBQ = (x: Seq[String]) => BlockQuote(flattenString(x.toVector))

    rule(blockQuoteLine.+ ~ (!blankLine ~ anyLine).* ~ blankLine.* ~> toBQ)
  }

  def heading: Rule1[HeadingBlock] = rule { atxHeading }

  def atxHeading: Rule1[HeadingBlock] = {
    def headingContents = rule(inline.+)
    @inline
    def h = (lev: Int) => rule {
      lev.times("#") ~ !endLine ~ !"#" ~ sp ~ capture(headingContents) ~ anyOf("# \t").* ~ nl.* ~> (HeadingBlock(lev, _))
    }
    rule { h(6) | h(5) | h(4) | h(3) | h(2) | h(1) }
  }

  def paragraph: Rule1[Paragraph] = rule(capture((inline | endLine).+) ~ blankLine.+ ~> Paragraph) // ToDo think if inline rule should include endLine as an ordered choice
  def plain: Rule1[Plain] = rule(capture(inline.+) ~ blankLine.? ~> Plain)
}