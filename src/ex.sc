import org.parboiled2._

sealed trait ListItemA
case class OrderedListItem(inline: String) extends ListItemA
case class UnorderedListItem(inline: String) extends ListItemA

class SimpleListParser(val input: ParserInput) extends Parser {
  import CharPredicate._

  def InputLine = rule(ListItem.+ ~ EOI)

  def ListItem: Rule1[ListItemA] = rule {
    Ordered ~ Inline ~ Newline ~> OrderedListItem |
    Ordered ~ Inline ~> OrderedListItem |
    Unordered ~ Inline ~ Newline ~> UnorderedListItem |
    Unordered ~ Inline ~> UnorderedListItem
  }

  def Ordered = rule { Digit.+ ~ "." ~ WS.+ }
  def Unordered = rule { ("*" | "-" | "+").+ ~ WS.+ }
  def Inline: Rule1[String] = rule { capture(AlphaNum.+) }
  def Newline = rule { "\r" ~ "\n" | "\n" }
  def WS = rule { " " | "\t" }
}

val s =
  """1. one
    |2. two
    |3. three""".stripMargin

new SimpleListParser(s).InputLine.run()