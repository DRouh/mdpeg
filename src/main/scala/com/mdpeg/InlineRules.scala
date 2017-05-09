package com.mdpeg

import org.parboiled2._

trait InlineRules {
  this: Parser with PrimitiveRules =>
  import CharPredicate._

  def inline: Rule1[Inline] = rule(strong | spaces | text)

  def text: Rule1[Text] = rule(capture(textChar.+) ~> (Text(_)))
  def spaces: Rule1[Space.type] = rule(capture(sp.+) ~> ((_:String) => Space))
  def strong: Rule1[Strong] = rule(strongStarred | strongUnderlined)

  def strongStarred: Rule1[Strong] = {
    def contents: Rule1[Seq[Inline]] = rule((!(spnl ~ twoStar) ~ inline).+)

    rule(twoStar ~ !sp ~ !nl ~ contents ~ twoStar ~> (Strong(_)))
  }

  def strongUnderlined: Rule1[Strong] = {
    def contents: Rule1[Seq[Inline]] = rule((!(spnl ~ twoUnder) ~ !twoUnder ~ inline).+)

    rule(twoUnder ~ !sp ~ !nl ~ contents ~ twoUnder ~ !AlphaNum ~> (Strong(_)))
  }


  def twoStar: Rule0 = rule("**" ~ !twoStar)
  def twoUnder: Rule0 = rule("__" ~ !twoUnder)

  def label: Rule1[String] = rule("[" ~ capture((!"]" ~ inlineChar).+) ~ "]") // [label]
  def title: Rule1[String] = rule { // 'title' or "title" for reference
    "\"" ~ capture((!"\"" ~ !nl ~ anyChar).*) ~ "\"" |
    "'" ~ capture((!"'" ~ !nl ~ anyChar).*) ~ "'"
  }
}
