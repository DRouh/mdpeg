package org.mdpeg.parsers

import org.mdpeg._
import org.mdpeg.ast._
import org.parboiled2._

private[mdpeg] class BlockParser(val input: ParserInput) extends Parser
  with PrimitiveRules
  with ListBlockRules
  with MultilineTablesRules
  with InlineRules {
  def InputLine: Rule1[Seq[Block]] = rule(block.+ ~ EOI)

  def block: Rule1[Block] = rule { blockQuote | tex | verbatim | reference | heading | list | multiTable | horizontalRule | paragraph | plain  }

  //block definitions
  def verbatim : Rule1[Verbatim] = {
    def bound = rule(3.times("`"))
    def anyCharVerbatim = rule(!nl ~ !EOI ~ !bound ~ ANY)
    def contents = rule((nl ~ !bound | anyCharVerbatim.+).+)
    rule (bound ~ nl ~ capture(contents) ~ nl ~ bound ~ blankLine.* ~> Verbatim)
  }

  def tex: Rule1[TexBlock] = {
    def bound = rule(3.times("$"))
    def anyCharTex = rule(!nl ~ !EOI ~ !bound ~ ANY)
    def contents = rule((nl ~ !bound | anyCharTex.+).+)
    rule (bound ~ nl ~ capture(contents) ~ nl ~ bound ~ blankLine.* ~> ((c: String) => TexBlock(TexContent(c))))
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
      rule(nonIndentSpace ~ ">" ~ sps ~ capture(anyLine) ~> ((s:String) => Markdown(RawMarkdownContent(trimEndWithEnding(s)))))
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