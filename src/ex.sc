import org.parboiled2._

val s =
  """1. one
    |2. two
    |3. three
    |""".stripMargin

sealed trait ListItemA

case class OrderedListItem(inline: String) extends ListItemA

case class UnorderedListItem(inline: String) extends ListItemA

class SimpleParser(val input: ParserInput) extends Parser {

  import CharPredicate._

  def InputLine = rule(ListItem.+ ~ EOI)

  def ListItem: Rule1[ListItemA] = rule {
    Ordered ~ Inline ~ Newline ~> OrderedListItem |
      Unordered ~ Inline ~ Newline ~> UnorderedListItem
  }

  def Ordered = rule {
    oneOrMore(Digit) ~ "." ~ (" " | "\t").+
  }

  def Unordered = rule {
    (("*" | "-" | "+") ~ (" " | "\t")).+
  }

  def Inline: Rule1[String] = rule {
    capture(AlphaNum.+.+)
  }

  def Newline = rule {
    "\r" ~ zeroOrMore("\n") | "\n"
  }
}

new SimpleParser(s).InputLine.run()