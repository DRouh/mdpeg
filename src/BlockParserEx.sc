import com.mdpeg._
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}
import org.parboiled2._

class BlockParser1(val input: ParserInput) extends Parser {
  import CharPredicate._
  def InputLine = rule(block.+ ~ EOI)

  def block : Rule1[Block] = rule { paragraph | plain  }

  //block definitions
  def horizontalRule = rule {
    nonIndentSpace ~ "-" ~ sp ~ "-" ~ sp ~ "-" ~ (sp ~ "-").* ~ sp ~ nl ~ blankLine.+
  }

  def paragraph  : Rule1[Paragraph] = rule { capture(inline.+) ~ nl ~ blankLine.+ ~> Paragraph }
  def plain : Rule1[Plain]     = rule { capture(inline.+) ~ blankLine.? ~> Plain }

  //aux functions
  def h: Rule1[Option[String]] = rule { optional(capture("   " | "  " | " ")) ~> (_ => _) }
  def nonIndentSpace : Rule1[String] = rule { h ~> (x => x.getOrElse("")) }

  def inline          = rule { AlphaNum | sp | punctuationChar | anyOf("_\"{}()") }
  def blankLine       = rule { sp.* ~ nl }
  def punctuationChar = rule { anyOf(":;,.?!-") }
  def nl              = rule { "\r\n" | "\r" | "\n" }
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
PrettyPrint1(new BlockParser1(input2))