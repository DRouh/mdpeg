package com.mdpeg

import org.parboiled2._

trait InlineRules {
  this: Parser with PrimitiveRules =>
  import CharPredicate._

  def inline: Rule1[Inline] = rule(strong | italics | endLine | spaces | text)
  def text:    Rule1[Text] = rule(capture(textChar.+) ~> Text)
  def endLine: Rule1[Space.type] = rule(capture(" ".? ~ nl ~ !blankLine ~ !EOI) ~> ((_:String) => Space))
  def spaces:  Rule1[Space.type] = rule(capture(sp.+) ~> ((_:String) => Space))


  def strong:  Rule1[Strong] = rule(strongStarred | strongUnderlined)

  def strongStarred: Rule1[Strong] = {
    def contents: Rule1[Seq[Inline]] = rule((!(spnl ~ twoStar) ~ inline).+)

    rule(twoStar ~ !sp ~ !nl ~ contents ~ twoStar ~> Strong)
  }
  def strongUnderlined: Rule1[Strong] = {
    def contents: Rule1[Seq[Inline]] = rule((!(spnl ~ twoUnder) ~ !twoUnder ~ inline).+)

    rule(twoUnder ~ !sp ~ !nl ~ contents ~ twoUnder ~ !AlphaNum ~> Strong)
  }
  def twoStar:  Rule0 = rule("**" ~ !twoStar)
  def twoUnder: Rule0 = rule("__" ~ !twoStar) //!twoStar to forbid having strong in strong

  // italics
  def italics: Rule1[Italics] = rule(italicsStarred|italicsUnderlined)
  def italicsStarred: Rule1[Italics] = {
    def contents: Rule1[Seq[Inline]] = rule((strong | !(spnl ~ oneStar) ~ !oneUnder ~ inline).+)
    rule(oneStar ~ !sp ~ !nl ~ contents ~ oneStar ~> Italics)
  }
  def italicsUnderlined: Rule1[Italics] = {
    def contents: Rule1[Seq[Inline]] = rule((strong | !(spnl ~ oneUnder) ~ !oneStar ~ inline).+)
    rule(oneUnder ~ !sp ~ !nl ~ contents ~ oneUnder ~ !AlphaNum ~> Italics)
  }
  def oneStar: Rule0 = rule("*" ~ !oneStar)
  def oneUnder: Rule0 = rule("_" ~ !oneUnder)

  def label: Rule1[Seq[Inline]] = rule("[" ~ (!"]" ~ inline).+ ~ "]") // [label]
  def title: Rule1[String] = rule { // 'title' or "title" for reference
    "\"" ~ capture((!"\"" ~ !nl ~ anyChar).*) ~ "\"" |
    "'" ~ capture((!"'" ~ !nl ~ anyChar).*) ~ "'"
  }
}
