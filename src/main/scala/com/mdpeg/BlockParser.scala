package com.mdpeg

import org.parboiled2._

class BlockParser(val input: ParserInput) extends Parser {

  import CharPredicate._

  def InputLine = rule(block.+ ~ EOI)

  def block: Rule1[Block] = rule { horizontalRule | paragraph | plain }

  //block definitions
  def horizontalRule: Rule1[HorizontalRuleBlock] = rule {
    nonIndentSpace ~ capture("-" ~ spOs ~ "-" ~ spOs ~ "-" ~ (spOs ~ "-").* ~ spOs ~ nl ~ blankLine.*) ~>
      ((x: String, y: String) => HorizontalRuleBlock(x + y))
  }

  def paragraph: Rule1[Paragraph] = rule { capture(inline.+) ~ nl ~ blankLine.+ ~> Paragraph }
  def plain: Rule1[Plain] = rule { capture(inline.+) ~ blankLine.? ~> Plain }

  //aux functions
  def nonIndentSpace: Rule1[String] = {
    // only to facilitate type inference,
    // i.e to support optional(A,B) where B returned when A is None
    def h(x: Any) = x match {
      case x: Option[String] => x.getOrElse("")
      case _ => ""
    }

    rule {
      capture("   " | "  " | " ").? ~> (h(_))
    }
  }

  def inline          = rule { AlphaNum | sp | punctuationChar | anyOf("_\"{}()") }
  def blankLine       = rule { sp.* ~ nl }
  def punctuationChar = rule { anyOf(":;,.?!-") }
  def nl              = rule { "\r\n" | "\r" | "\n" }
  def spOs            = rule { sp.* }
  def sp              = rule { " " | "\t" }
}
