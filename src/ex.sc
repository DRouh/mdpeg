import org.parboiled2._

import scala.collection.immutable
import scala.util.{Failure, Success, Try}

object PrettyPrint {
  def apply(parser : SimpleListParser) : Unit= {
    val result: Try[immutable.Seq[ListItem]] = parser.InputLine.run()
    result match {
      case Failure(error) =>
        error match {
          case e : ParseError => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
          case _ => println(error)
        }
      case Success(value) => println(value)
    }

  }
}
sealed trait ListItem
case class OrderedListItem(inline: String) extends ListItem
case class UnorderedListItem(inline: String) extends ListItem

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
  def Unordered = rule { UnorderedChar.+ ~ WS.+ }
  def UnorderedChar = rule {"*" | "-" | "+"}
  def Inline: Rule1[String] = rule { capture((InlineChar.+ ~ WS.*).*) }
  def InlineChar = rule {AlphaNum | anyOf(":;,.?!_-'\"{}")}
  def Newline = rule { "\r" ~ "\n" | "\n" }
  def WS = rule { " " | "\t" }
}

val input1 = """1. It is a long - established fact that a reader will be;"""
val input2 =
  """1. 1. It is a long - established fact that a reader will be;
    |2. The point of using - Lorem Ipsum is that it has a more-or-less normal ;
    |3. Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for 'lorem ipsum'.""".stripMargin

PrettyPrint(new SimpleListParser(input1))
PrettyPrint(new SimpleListParser(input2))