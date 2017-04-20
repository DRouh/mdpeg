package com.mdpeg

import org.parboiled2.{CharPredicate, Parser, ParserInput, Rule1}

class SimpleListParser(val input: ParserInput) extends Parser {
  import CharPredicate._

  def InputLine = rule(AnyListItem.+ ~ EOI)

  def AnyListItem: Rule1[ListItem] = rule {
    Ordered ~ Inline ~ Newline ~> OrderedListItem |
      Ordered ~ Inline ~> OrderedListItem |
      Unordered ~ Inline ~ Newline ~> UnorderedListItem |
      Unordered ~ Inline ~> UnorderedListItem
  }

  def Ordered = rule { Digit.+ ~ "." ~ WS.+ }

  def WS = rule { " " | "\t" }

  def Unordered = rule { UnorderedChar.+ ~ WS.+ }

  def UnorderedChar = rule {"*" | "-" | "+"}

  def Inline: Rule1[String] = rule { capture((InlineChar.+ ~ WS.*).*) }

  def InlineChar = rule {AlphaNum | anyOf(":;,.?!_-'\"{}")}

  def Newline = rule { "\r" ~ "\n" | "\r" | "\n" }
}
