package com.mdpeg

import org.parboiled2._

class BlockParser(val input: ParserInput) extends PrimitiveRules {

  def InputLine = rule(block.+ ~ EOI)

  def block : Rule1[Block] = rule { blockQuote | verbatim | heading | horizontalRule | paragraph | plain  }

  //block definitions
  def verbatim : Rule1[Verbatim] = {
    def math = rule { anyOf("=/\\*-+^%!<>[]{}") }
    def other = rule {anyOf("@#$\"“")}
    def special1 = rule { math | other }
    def inlineCodeChar = rule( inline | special1 ) // ToDo add blank line?

    rule {
      3.times("`") ~ capture((nl | inlineCodeChar.+ ~ nl).+) ~3.times("`") ~ blankLine.* ~> Verbatim
    }
  }

  def blockQuote : Rule1[BlockQuote] = {
    def blockQuoteLine :Rule1[String]= rule {
      nonIndentSpace ~ ">" ~ sp.+ ~ capture(anyLine)
    }

    def toBQ = (x: Any) => BlockQuote(flattenString(x.asInstanceOf[Vector[String]]))

    rule {
      blockQuoteLine.+ ~ (!blankLine ~ anyLine).* ~ blankLine.* ~> toBQ
    }
  }

  def heading = rule { atxHeading }

  def atxHeading: Rule1[HeadingBlock] = {
    @inline
    def h = (lev: Int) => rule {
      lev.times("#") ~ (!endLine ~ !"#" ~ sp ~ capture(inline.+)) ~ anyOf("# \t").* ~ nl.* ~> (HeadingBlock(lev, _))
    }
    rule { h(6) | h(5) | h(4) | h(3) | h(2) | h(1) }
  }

  def horizontalRule: Rule1[HorizontalRuleBlock.type] = {
    @inline
    def h = (ch: String) => rule { ch ~ spOs ~ ch ~ spOs ~ ch ~ (spOs ~ ch).* ~ spOs ~ nl ~ blankLine.+ }
    def toHr = (_: String) => HorizontalRuleBlock
    rule { nonIndentSpace ~ capture(h("-") | h("*") | h("_")) ~> toHr }
  }

  def paragraph : Rule1[Paragraph] = rule { capture((inline | endLine).+) ~ blankLine.+ ~> Paragraph } // ToDo think if inline rule should include endLine as an ordered choice
  def plain : Rule1[Plain] = rule { capture(inline.+) ~ blankLine.? ~> Plain }

  //utility methods
  /**
    * Flattens a vector of strings into one string, replaces all cr/crlf with spaces
    * @param xs a vector of strings
    * @return a flat string composed of original list
    * */
  private def flattenString(xs:Vector[String]) = xs.mkString(" ").replace("\r\n","").replace("\r", "").replace("\n", "")
}