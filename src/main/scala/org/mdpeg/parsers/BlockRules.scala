package org.mdpeg.parsers

import org.mdpeg.ast._
import org.mdpeg.Pipe
import org.mdpeg.trimEndWithEnding
import org.parboiled2.CharPredicate.AlphaNum
import org.parboiled2.{Parser, Rule1}

private[mdpeg] trait BlockRules {
  this: Parser with PrimitiveRules with InlineRules =>

  def verbatim: Rule1[Verbatim] = {
    def bound = rule(3.times("`"))

    def anyCharVerbatim = rule(!nl ~ !EOI ~ !bound ~ ANY)

    def contents = rule((nl ~ !bound | anyCharVerbatim.+).+)

    def syntaxChar = rule(atomic(AlphaNum | anyOf("_\"[]{}()^%@#$:;,.?!’“”—=/\\*-+<>")))

    def syntax = rule(sps ~ capture((syntaxChar | sps ~ syntaxChar).+) ~ sps)

    rule(bound ~ syntax.? ~ nl ~ capture(contents) ~ nl ~ bound ~ blankLine.* ~> ((s: Option[String], c: String) => Verbatim(c, s)))
  }

  def tex: Rule1[TexBlock] = {
    def bound = rule(3.times("$"))

    def anyCharTex = rule(!nl ~ !EOI ~ !bound ~ ANY)

    def contents = rule((nl ~ !bound | anyCharTex.+).+)

    rule(bound ~ nl ~ capture(contents) ~ nl ~ bound ~ blankLine.* ~> ((c: String) => TexBlock(TexContent(c))))
  }

  def reference: Rule1[ReferenceBlock] = {
    def uri = rule((!sp ~ !nl ~ !EOI ~ ANY).+)

    /*_*/
    rule(nonIndentSpace ~ label ~ ":" ~ spnl ~ capture(uri) ~ (spnl ~ title).? ~ blankLine.* ~>
      ((label: Seq[Inline], uri: String, title: Option[String]) => ReferenceBlock(label, Src(uri, title))))
    /*_*/
  }

  def blockQuote: Rule1[BlockQuote] = {
    def blockQuoteLine: Rule1[Markdown] = {
      rule(nonIndentSpace ~ ">" ~ sps ~ capture(anyLine) ~>
        ((s: String) => s |> trimEndWithEnding |> RawMarkdownContent |> Markdown))
    }

    rule(blockQuoteLine.+ ~ (!blankLine ~ anyLine).* ~ blankLine.* ~> ((x: Seq[Markdown]) => BlockQuote(x.toVector)))
  }

  def heading: Rule1[HeadingBlock] = rule(atxHeading)

  private def atxHeading: Rule1[HeadingBlock] = {
    def h = (lev: Int) => rule {
      lev.times("#") ~ !endLine ~ !"#" ~ sp ~ inline.+ ~ anyOf("# \t").* ~ nl.* ~> (HeadingBlock(lev, _))
    }

    rule(h(6) | h(5) | h(4) | h(3) | h(2) | h(1))
  }

  // ToDo think if inline rule should include endLine as an ordered choice

  def paragraph: Rule1[Paragraph] = rule(inline.+ ~ nl ~ blankLine.+ ~> Paragraph)

  def plain: Rule1[Plain] = rule(inline.+ ~ blankLine.? ~> Plain)
}
