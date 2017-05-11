package com.mdpeg

import org.parboiled2._

trait InlineRules {
  this: Parser with PrimitiveRules =>
  import CharPredicate._

  def inline:  Rule1[Inline] = rule(strong | italics | endLine | link | autolink | spaces | text)

  def text:     Rule1[Text]       = rule(capture(textChar.+) ~> Text)
  def endLine:  Rule1[Space.type] = rule(capture(" ".? ~ nl ~ !blankLine ~ !EOI) ~> ((_:String) => Space))
  def spaces:   Rule1[Space.type] = rule(capture(sp.+) ~> ((_:String) => Space))
  def strong:   Rule1[Strong]     = rule(strongStarred | strongUnderlined)
  def italics:  Rule1[Italics]    = rule(italicsStarred | italicsUnderlined)
  def link:     Rule1[Link]       = rule(explicitLink | referenceLink)
  def autolink: Rule1[Link]       = rule(autolinkUri | autolinkEmail)

  def explicitLink:  Rule1[Link] = {
    def excludeChars: Rule0 = rule(noneOf("()> \r\n\t"))
    def source:  Rule1[String] = rule("<" ~ capture(source1) ~ ">" | capture(source1))
    def source1: Rule0 = rule(zeroOrMore(excludeChars.+ | ("(" ~ source1 ~ ")") | ("<" ~ source1 ~ ">")))

    /*_*/
    def sourceAndTitle: Rule1[(String, Option[String])] = {
      rule("(" ~ sps ~ source ~ spnl ~ title.? ~ sps ~ ")" ~> ((s: String, o: Option[String]) => (s, o)))
    }
    /*_*/

    rule(label ~ spnl ~ sourceAndTitle ~> ((inline: Seq[Inline], uriTitle: (String, Option[String])) =>
      uriTitle match {
        case (uri:String, None) => Link(inline, Src(uri, None))
        case (uri:String, title:Option[String]) => Link(inline, Src(uri, title))
        case _ => Link(inline, Src(uriTitle._1, None))
      })
    )
  }
  def referenceLink: Rule1[Link] = {
    rule(label ~ capture(spnl) ~ label ~> ((l1: Seq[Inline], s:String, l2: Seq[Inline]) => Link(l1, Ref(l2, s))) |
         label ~> ((l: Seq[Inline]) => Link(l, ShortcutRef)))
  }
  def autolinkUri:   Rule1[Link] = {
    rule("<" ~ capture(Alpha.+ ~ "://") ~ capture((!nl ~ !">" ~ ANY).+) ~ ">" ~>
      ((protocol: String, link: String) => Link(Vector(Text(protocol+link)), Src(protocol+link, None))))
  }
  def autolinkEmail: Rule1[Link] = {
    rule("<" ~ capture(Alpha.+ ~ "@" ~ (!nl ~ !">" ~ ANY).+) ~ ">" ~> ((s:String)=> Link(Vector(Text(s)), Src("mailto:" +s, None))))
  }

  def label: Rule1[Seq[Inline]] = rule("[" ~ (!"]" ~ inline).+ ~ "]") // [label]
  def title: Rule1[String] = rule { // 'title' or "title" for reference
    "\"" ~ capture((!"\"" ~ !nl ~ anyChar).*) ~ "\"" |
      "'" ~ capture((!"'" ~ !nl ~ anyChar).*) ~ "'"
  }

  def strongStarred:     Rule1[Strong]  = {
    def contents: Rule1[Seq[Inline]] = rule((!(spnl ~ twoStar) ~ inline).+)
    rule(twoStar ~ !sp ~ !nl ~ contents ~ twoStar ~> Strong)
  }
  def strongUnderlined:  Rule1[Strong]  = {
    def contents: Rule1[Seq[Inline]] = rule((!(spnl ~ twoUnder) ~ !twoUnder ~ inline).+)
    rule(twoUnder ~ !sp ~ !nl ~ contents ~ twoUnder ~ !AlphaNum ~> Strong)
  }
  def italicsStarred:    Rule1[Italics] = {
    def contents: Rule1[Seq[Inline]] = rule((strong | !(spnl ~ oneStar) ~ !oneUnder ~ inline).+)
    rule(oneStar ~ !sp ~ !nl ~ contents ~ oneStar ~> Italics)
  }
  def italicsUnderlined: Rule1[Italics] = {
    def contents: Rule1[Seq[Inline]] = rule((strong | !(spnl ~ oneUnder) ~ !oneStar ~ inline).+)
    rule(oneUnder ~ !sp ~ !nl ~ contents ~ oneUnder ~ !AlphaNum ~> Italics)
  }

  def twoStar:  Rule0 = rule("**" ~ !twoStar)
  def twoUnder: Rule0 = rule("__" ~ !twoStar) //!twoStar to forbid having strong in strong
  def oneStar:  Rule0 = rule("*" ~ !oneStar)
  def oneUnder: Rule0 = rule("_" ~ !oneUnder)
}
