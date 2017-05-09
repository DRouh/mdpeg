package com.mdpeg

import org.parboiled2._

class BlockParser(val input: ParserInput) extends Parser
  with PrimitiveRules
  with ListBlockRules
  with MultilineTablesRules
  with InlineRules {
  def InputLine: Rule1[Seq[Block]] = rule(block.+ ~ EOI)

  def block: Rule1[Block] = rule { blockQuote | verbatim | reference | heading | list | multiTable | horizontalRule | paragraph | plain  }

  //block definitions
  def verbatim : Rule1[Verbatim] = {
    def inlineCodeChar = rule( inlineChar | mathChar | specialChar ) // ToDo add blank line?
    def verbatimBlockBound = rule(3.times("`"))
    def verbatimBlockContents = rule((nl | inlineCodeChar.+ ~ nl).+)
    rule (verbatimBlockBound ~ capture(verbatimBlockContents) ~ verbatimBlockBound ~ blankLine.* ~> Verbatim)
  }

  /*_*/
  def reference: Rule1[ReferenceBlock] = {
    def uri = rule((!sp ~ !nl ~ anyChar).+)
    rule(nonIndentSpace ~ label ~ ":" ~ spnl ~ capture(uri) ~ (spnl ~ title).? ~ blankLine.* ~>
      ((label:String, uri:String, title:Option[String]) => ReferenceBlock(label, Src(uri, title))))
  }
  /*_*/

  def blockQuote : Rule1[BlockQuote] = { // ToDo think if should store a Markdown instead of concatenated strings
    def blockQuoteLine: Rule1[String] = rule(nonIndentSpace ~ ">" ~ sp.+ ~ capture(anyLine))
    // ToDo think if keep line breaks or keep as it is (i.e. replaced with spaces)
    def toBQ = (x: Seq[String]) => BlockQuote(flattenString(x.toVector))

    rule(blockQuoteLine.+ ~ (!blankLine ~ anyLine).* ~ blankLine.* ~> toBQ)
  }

  def heading: Rule1[HeadingBlock] = rule { atxHeading }

  def atxHeading: Rule1[HeadingBlock] = {
    def headingContents = rule(inlineChar.+)
    @inline
    def h = (lev: Int) => rule {
      lev.times("#") ~ !endLine ~ !"#" ~ sp ~ capture(headingContents) ~ anyOf("# \t").* ~ nl.* ~> (HeadingBlock(lev, _))
    }
    rule { h(6) | h(5) | h(4) | h(3) | h(2) | h(1) }
  }

  // ToDo think if inline rule should include endLine as an ordered choice
  def paragraph: Rule1[Paragraph] = rule(capture((inlineChar | endLine).+) ~ blankLine.+ ~> Paragraph)
  def plain: Rule1[Plain] = rule(capture(inlineChar.+) ~ blankLine.? ~> Plain)
}