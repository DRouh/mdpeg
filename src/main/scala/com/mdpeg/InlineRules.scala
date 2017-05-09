package com.mdpeg

import org.parboiled2._

trait InlineRules {
  this: Parser with PrimitiveRules =>
  import CharPredicate._

  def inline: Rule1[Inline] = rule(strong)

  // strong
  def strong: Rule1[Strong] = rule(strongStarred | strongUnderlined)
  def strongStarred: Rule1[Strong] = {
    def contents: Rule0 = rule((!(spnl ~ twoStar) ~ inlineChar).+)

    //ToDo instead of inlineChar there should be recursive call to inline rule
    rule(twoStar ~ !sp ~ !nl ~ capture(contents) ~ twoStar ~> (Strong(_)))
  }
  def strongUnderlined: Rule1[Strong] = {
    def contents: Rule0 = rule((!(spnl ~ twoUnder) ~ !twoUnder ~ inlineChar).+)

    //ToDo instead of inlineChar there should be recursive call to inline rule
    rule(twoUnder ~ !sp ~ !nl ~ capture(contents) ~ twoUnder ~ !AlphaNum ~> (Strong(_)))
  }
  def twoStar: Rule0 = rule("**" ~ !twoStar)
  def twoUnder: Rule0 = rule("__" ~ !twoUnder)

  // italics
  def italics: Rule1[Italics] = rule(italicsStarred|italicsUnderlined)
  def italicsStarred: Rule1[Italics] = {
    def contents= rule((strong | !(spnl ~ oneStar) ~ inlineChar).+)

    ???
  }

  def italicsUnderlined: Rule1[Italics] = ???
  def oneStar: Rule0 = rule("*" ~ !oneStar)
  def oneUnder: Rule0 = rule("_" ~ !oneUnder)

  def label: Rule1[String] = rule("[" ~ capture((!"]" ~ inlineChar).+) ~ "]") // [label]
  def title: Rule1[String] = rule { // 'title' or "title" for reference
    "\"" ~ capture((!"\"" ~ !nl ~ anyChar).*) ~ "\"" |
    "'" ~ capture((!"'" ~ !nl ~ anyChar).*) ~ "'"
  }
}
