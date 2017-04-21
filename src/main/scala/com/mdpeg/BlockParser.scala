package com.mdpeg

import org.parboiled2._

class BlockParser(val input: ParserInput) extends Parser {
  import CharPredicate._
  def InputLine = rule(paragraph.+ ~ EOI)

  def paragraph = rule { inline.+ ~ nl ~ blankLine.+ }

  //aux functions
  def inline = rule {AlphaNum | sp | punctuationChar | anyOf("_\"{}()")}
  def blankLine = rule { capture(sp ~ nl) }
  def punctuationChar = rule {anyOf(":;,.?!-") }
  def nl = rule { "\r" ~ "\n" | "\r" | "\n" }
  def sp = rule { " "| "\t" }
}
