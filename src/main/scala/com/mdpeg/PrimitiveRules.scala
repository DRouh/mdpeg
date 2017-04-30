package com.mdpeg

import org.parboiled2._

trait PrimitiveRules extends Parser {
  import CharPredicate._

  def horizontalRule: Rule1[HorizontalRuleBlock.type] = {
    @inline
    def h = (ch: String) => rule(ch ~ spaces ~ ch ~ spaces ~ ch ~ (spaces ~ ch).* ~ spaces ~ nl ~ blankLine.+)
    def toHr = (_: String) => HorizontalRuleBlock
    rule(nonIndentSpace ~ capture(h("-") | h("*") | h("_")) ~> toHr)
  }

  def indentedLine    : Rule0 = rule(indent ~ anyLine)
  def anyLine         : Rule0 = rule(!nl ~ !EOI ~ inline.+ ~ (nl | ""))
  def endLine         : Rule0 = rule(sp.? ~ nl ~ !blankLine ~ !EOI)
  def indent          : Rule0 = rule("\t" | "    ")
  def nonIndentSpace  : Rule0 = rule("   " | "  " | " " | "")
  def inline          : Rule0 = rule(AlphaNum | sp | punctuationChar | anyOf("_\"{}()'"))
  def blankLine       : Rule0 = rule(sp.* ~ nl)
  def punctuationChar : Rule0 = rule(anyOf(":;,.?!-’“”—")) // ToDo think how to handle backtick '`' so that it is not confused with verbatim block
  def nl              : Rule0 = rule("\r\n" | "\r" | "\n")
  def spaces          : Rule0 = rule(sp.*)
  def sp              : Rule0 = rule(" " | "\t")
}
