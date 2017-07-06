package org.mdpeg

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
    def verbatimBlockBound = rule(3.times("`"))
    def anyCharVerbatim: Rule0 = rule(!nl ~ !EOI ~ !verbatimBlockBound ~ ANY)
    def verbatimBlockContents = rule((nl | anyCharVerbatim.+ ~ nl).+)
    rule (verbatimBlockBound ~ capture(verbatimBlockContents) ~ verbatimBlockBound ~ blankLine.* ~> Verbatim)
  }

  /*_*/
  def reference: Rule1[ReferenceBlock] = {
    def uri = rule((!sp ~ !nl ~ !EOI ~ ANY).+)
    rule(nonIndentSpace ~ label ~ ":" ~ spnl ~ capture(uri) ~ (spnl ~ title).? ~ blankLine.* ~>
      ((label:Seq[Inline], uri:String, title:Option[String]) => ReferenceBlock(label, Src(uri, title))))
  }
  /*_*/

  def blockQuote : Rule1[BlockQuote] = {
    def blockQuoteLine: Rule1[Markdown] = {
      rule(nonIndentSpace ~ ">" ~ sps ~ capture(anyLine) ~> ((s:String) => Markdown(trimEndWithEnding(s))))
    }
    rule(blockQuoteLine.+ ~ (!blankLine ~ anyLine).* ~ blankLine.* ~> ((x: Seq[Markdown]) => BlockQuote(x.toVector)))
  }

  def heading: Rule1[HeadingBlock] = rule(atxHeading)

  private def atxHeading: Rule1[HeadingBlock] = {
    def headingContents = rule(inline.+)
    @inline
    def h = (lev: Int) => rule {
      lev.times("#") ~ !endLine ~ !"#" ~ sp ~ headingContents ~ anyOf("# \t").* ~ nl.* ~> (HeadingBlock(lev, _))
    }
    rule(h(6) | h(5) | h(4) | h(3) | h(2) | h(1))
  }

  // ToDo think if inline rule should include endLine as an ordered choice

  def paragraph: Rule1[Paragraph] = rule(inline.+ ~ nl ~ blankLine.+ ~> Paragraph)

  def plain: Rule1[Plain] = rule(inline.+ ~ blankLine.? ~> Plain)
}