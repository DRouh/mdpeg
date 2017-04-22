package com.mdpeg

import org.parboiled2._

class BlockParser(val input: ParserInput) extends Parser {
  import CharPredicate._
  def InputLine = rule(block.+ ~ EOI)

  def block : Rule1[Block] = rule { para | plain  }

  //block definitions
  def para  : Rule1[Para]      = rule { capture(inline.+) ~ nl ~ blankLine.+ ~> Para }
  def plain : Rule1[Paragraph] = rule { capture(inline.+) ~ blankLine.? ~> Paragraph }

  //aux functions
  //def inline          = rule { AlphaNum | sp | nl | punctuationChar | anyOf("_\"{}()") }
  def inline          = rule { AlphaNum | sp | punctuationChar | anyOf("_\"{}()") }
  def blankLine       = rule { sp.* ~ nl }
  def punctuationChar = rule { anyOf(":;,.?!-") }
  def nl              = rule { "\r\n" | "\r" | "\n" }
  def sp              = rule { " " | "\t" }
}
