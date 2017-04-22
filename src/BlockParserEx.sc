import com.mdpeg._
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}
import org.parboiled2._

class BlockParser1(val input: ParserInput) extends Parser {
  import CharPredicate._
  def InputLine = rule(block.+ ~ EOI)

  def block : Rule1[Block] = rule { horizontalRule | paragraph | plain  }

  //block definitions
  def horizontalRule : Rule1[HorizontalRuleBlock] = rule {
    nonIndentSpace ~ capture(horizontalRuleWithCh("-") | horizontalRuleWithCh("*") | horizontalRuleWithCh("_")) ~>
      ((x:String, y:String) => HorizontalRuleBlock(x+y))
  }

  def paragraph : Rule1[Paragraph] = rule { capture(inline.+) ~ nl ~ blankLine.+ ~> Paragraph }
  def plain : Rule1[Plain]         = rule { capture(inline.+) ~ blankLine.? ~> Plain }

  //aux rules
  private def horizontalRuleWithCh = (sep: String) => rule { sep ~ spOs ~ sep ~ spOs ~ sep ~ (spOs ~ sep).* ~ spOs ~ nl ~ blankLine.*}

  //primitives
  def nonIndentSpace: Rule1[String] = {
    // only to facilitate type inference,
    // i.e to support optional(A,B) where B returned when A is None
    def h(x: Any) = x match {
      case x: Option[String] => x.getOrElse("")
      case _ => ""
    }

    rule {
      capture("   " | "  " | " ").? ~> (h(_))
    }
  }

  def inline          = rule { AlphaNum | sp | punctuationChar | anyOf("_\"{}()") }
  def blankLine       = rule { sp.* ~ nl }
  def punctuationChar = rule { anyOf(":;,.?!-") }
  def nl              = rule { "\r\n" | "\r" | "\n" }
  def spOs            = rule { sp.* }
  def sp              = rule { " " | "\t" }
}


object PrettyPrint1 {
  def apply(parser : BlockParser1) : Unit= {
    val result= parser.InputLine.run()
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
val input1 =
  "It is a long established fact that a reader will be distracted by the\r\n \r\n"
val input2 =
  "It is a long established fact that a reader will be distracted by the"

//horizontal rule
//captures horizontal rule preceded by a non indented space (that is, up to 3 spaces)
val input3 = "   ____\r\n" // captures
PrettyPrint1(new BlockParser1(input3))