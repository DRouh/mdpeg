package com.mdpeg

import org.parboiled2._

class BlockParser(val input: ParserInput) extends PrimitiveRules {
  def InputLine = rule(block.+ ~ EOI)

  def block : Rule1[Block] = rule { blockQuote | heading | horizontalRule | paragraph | plain  }

  //block definitions
  def blockQuote : Rule1[BlockQuote] = {
    def blockQuoteLine :Rule1[String]= rule {
      nonIndentSpace ~ ">" ~ sp.* ~ capture(anyLine) ~> ((_:Any, z:String) => z)
    }

    def toBQ = (x: Any) => {
      val xs = x.asInstanceOf[Vector[String]]
      BlockQuote(xs.reduce(_+_) )
    }

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
    def toHr: (String, String) => HorizontalRuleBlock.type = (_: String, _: String) => HorizontalRuleBlock
    rule { nonIndentSpace ~ capture(h("-") | h("*") | h("_")) ~> toHr }
  }

  def paragraph : Rule1[Paragraph] = rule { capture((inline | endLine).+) ~ blankLine.+ ~> Paragraph } // ToDo think if inline rule should include endLine as an ordered choice
  def plain : Rule1[Plain] = rule { capture(inline.+) ~ blankLine.? ~> Plain }
}