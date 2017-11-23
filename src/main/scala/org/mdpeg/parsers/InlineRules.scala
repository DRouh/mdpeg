package org.mdpeg.parsers

import org.mdpeg.Pipe
import org.mdpeg.ast._
import org.parboiled2._

private[mdpeg] trait InlineRules {
  this: Parser with PrimitiveRules =>

  import CharPredicate._

  def inline: Rule1[Inline] =
    rule(strong | italics | texInline | code | linebreak | endLine | spaces | link | image | autolink | text | special)

  def text: Rule1[Text] = rule(capture(textChar.+) ~> Text)

  def endLine: Rule1[Space.type] = rule(capture(" ".? ~ nl ~ !blankLine ~ !EOI) ~> ((_: String) => Space))

  def linebreak: Rule1[LineBreak.type] = rule("  " ~ sps ~ endLine ~> ((_: Any) => LineBreak))

  def special: Rule1[Text] = rule(capture(specialChar) ~> Text)

  def spaces: Rule1[Space.type] = rule(capture(sp.+) ~> ((_: String) => Space))

  def strong: Rule1[Strong] = {
    def twoStar: Rule0 = rule("**" ~ !twoStar)

    def twoUnder: Rule0 = rule("__" ~ !twoStar) //!twoStar to forbid having strong in strong
    def strongStarred: Rule1[Strong] = {
      def contents: Rule1[Seq[Inline]] = rule((!(spnl ~ twoStar) ~ inline).+)

      rule(twoStar ~ !sp ~ !nl ~ contents ~ twoStar ~> Strong)
    }

    def strongUnderlined: Rule1[Strong] = {
      def contents: Rule1[Seq[Inline]] = rule((!(spnl ~ twoUnder) ~ !twoUnder ~ inline).+)

      rule(twoUnder ~ !sp ~ !nl ~ contents ~ twoUnder ~ !AlphaNum ~> Strong)
    }

    rule(strongStarred | strongUnderlined)
  }

  def italics: Rule1[Italics] = {
    def oneStar: Rule0 = rule("*" ~ !oneStar)

    def oneUnder: Rule0 = rule("_" ~ !oneUnder)

    def italicsStarred: Rule1[Italics] = {
      def contents: Rule1[Seq[Inline]] = rule((strong | !(spnl ~ oneStar) ~ !oneUnder ~ inline).+)

      rule(oneStar ~ !sp ~ !nl ~ contents ~ oneStar ~> Italics)
    }

    def italicsUnderlined: Rule1[Italics] = {
      def contents: Rule1[Seq[Inline]] = rule((strong | !(spnl ~ oneUnder) ~ !oneStar ~ inline).+)

      rule(oneUnder ~ !sp ~ !nl ~ contents ~ oneUnder ~ !AlphaNum ~> Italics)
    }

    rule(italicsStarred | italicsUnderlined)
  }

  def link: Rule1[Link] = {
    def explicitLink: Rule1[Link] = {
      def excludeChars: Rule0 = rule(noneOf("()> \r\n\t"))

      def source: Rule1[String] = rule("<" ~ capture(source1) ~ ">" | capture(source1))

      def source1: Rule0 = rule(zeroOrMore(excludeChars.+ | ("(" ~ source1 ~ ")") | ("<" ~ source1 ~ ">")))

      /*_*/
      def sourceAndTitle: Rule1[(String, Option[String])] = {
        rule("(" ~ sps ~ source ~ spnl ~ title.? ~ sps ~ ")" ~> ((s: String, o: Option[String]) => (s, o)))
      }

      /*_*/

      rule(label ~ spnl ~ sourceAndTitle ~> ((inline: Seq[Inline], uriTitle: (String, Option[String])) =>
        uriTitle match {
          case (uri: String, None) => Link(inline, Src(uri, None))
          case (uri: String, title: Option[String]) => Link(inline, Src(uri, title))
          case _ => Link(inline, Src(uriTitle._1, None))
        })
      )
    }

    def referenceLink: Rule1[Link] = {
      rule{
        label ~ capture(spnl) ~ label ~> ((l1: Seq[Inline], s: String, l2: Seq[Inline]) => Link(l1, Ref(l2, s))) |
        label ~> ((l: Seq[Inline]) => Link(l, ShortcutRef))
      }
    }

    rule(explicitLink | referenceLink)
  }

  def autolink: Rule1[Link] = {
    def autolinkUri: Rule1[Link] = {
      rule("<" ~ capture(Alpha.+ ~ "://") ~ capture((!nl ~ !">" ~ ANY).+) ~ ">" ~>
        ((protocol: String, link: String) => Link(Vector(Text(protocol + link)), Src(protocol + link, None))))
    }

    def autolinkEmail: Rule1[Link] = {
      rule("<" ~ capture(Alpha.+ ~ "@" ~ (!nl ~ !">" ~ ANY).+) ~ ">" ~>
        ((s: String) => Link(Vector(Text(s)), Src("mailto:" + s, None))))
    }

    rule(autolinkUri | autolinkEmail)
  }

  def image: Rule1[Image] = {
    def width: Rule1[Int] = rule {
      "{" ~ sps ~ ("width" | "WIDTH") ~ sps ~ "=" ~ sps ~ capture(Digit.+) ~ "%" ~ sps ~ "}" ~> ((w: String) => w.toInt)
    }

    /*_*/
    rule("!" ~ link ~ sps ~ width.? ~> ((l: Link, w: Option[Int]) => l match {
      case Link(inline, target) => Image(inline, target, w)
      case _ => sys.error("Error trying convert Link to image in patter matching expression.")
    }))
    /*_*/
  }

  def code: Rule1[Code] = {
    def ticks: Int => Rule0 = (n: Int) => rule(n.times("`") ~ !"`")

    def betweenTicks: Int => Rule1[String] = (n: Int) => rule {
      ticks(n) ~ capture((noneOf("`").+ | !ticks(n) ~ oneOrMore("`")).+) ~ ticks(n)
    }

    rule {
      &("`") ~ (betweenTicks(10) | betweenTicks(9) | betweenTicks(8) | betweenTicks(7) |
        betweenTicks(6) | betweenTicks(5) | betweenTicks(4) | betweenTicks(3) | betweenTicks(2) | betweenTicks(1)) ~>
        ((c: String) => Code(CodeContent(c)))
    }
  }

  def texInline: Rule1[TexInline] = {
    def bound = rule(2.times("$"))
    def betweenTicks: Rule1[String] = rule {
      bound ~ capture((noneOf("$").+ | !bound ~ oneOrMore("$")).+) ~ bound
    }

    rule { &("$") ~ betweenTicks ~> ((c: String) => c |> TexContent |> TexInline) }
  }

  def label: Rule1[Seq[Inline]] = rule("[" ~ (!"]" ~ inline).+ ~ "]") // [label]
  def title: Rule1[String] = rule { // 'title' or "title" for reference
    "\"" ~ capture((!"\"" ~ !nl ~ anyChar).*) ~ "\"" | "'" ~ capture((!"'" ~ !nl ~ anyChar).*) ~ "'"
  }
}