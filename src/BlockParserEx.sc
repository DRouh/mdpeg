import com.mdpeg._
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}
import org.parboiled2._

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

class BlockParser1(val input: ParserInput) extends Parser {
  import CharPredicate._
  def InputLine = rule(block.+ ~ EOI)

  def block : Rule1[Block] = rule { blockQuote | heading | horizontalRule | paragraph | plain  }

  //block definitions
  def blockQuote : Rule1[BlockQuote] = {
    def blockQuoteLine :Rule1[String]= rule {
      nonIndentSpace ~ ">" ~ sp.* ~ capture(anyLine) ~> ((_:Any, z:String) => z)
    }

    def toBQ = (x: Any) => {
      val xs = x.asInstanceOf[Vector[String]]
      BlockQuote(xs.reduce(_+_) )
    }

    rule {
      blockQuoteLine.+ ~ (!blankLine ~ anyLine).* ~ blankLine.* ~> toBQ
    }
  }

  def heading = rule { atxHeading }

  def atxHeading: Rule1[HeadingBlock] = {
    @inline
    def h = (lev: Int) => rule {
      lev.times("#") ~ (!endLine ~ !"#" ~ sp ~ capture(inline.+)) ~ anyOf("# \t").* ~ nl ~> (HeadingBlock(lev, _))
    }
    rule { h(6) | h(5) | h(4) | h(3) | h(2) | h(1) }
  }

  def horizontalRule: Rule1[HorizontalRuleBlock] = {
    @inline
    def h = (ch: String) => rule { ch ~ spOs ~ ch ~ spOs ~ ch ~ (spOs ~ ch).* ~ spOs ~ nl ~ blankLine.* }
    def toHr = (x: String, y: String) => HorizontalRuleBlock(x + y)
    rule { nonIndentSpace ~ capture(h("-") | h("*") | h("_")) ~> toHr }
  }

  def paragraph : Rule1[Paragraph] = rule { capture(inline.+) ~ nl ~ blankLine.+ ~> Paragraph }
  def plain : Rule1[Plain] = rule { capture(inline.+) ~ blankLine.? ~> Plain }

  //primitives
  def anyLine = rule { !nl ~ !EOI ~ inline.+ ~ (nl | "") }
  def endLine = rule { sp.? ~ nl ~ capture(!blankLine) ~ capture(!EOI) }

  def nonIndentSpace: Rule1[String] = {
    // only to facilitate type inference,
    // i.e to support optional(A,B) where B returned when A is None
    @inline
    def h(x: AnyRef): String = x match {
      case x: Option[String @unchecked] => x.getOrElse("")
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

//tests
val input1 =
  "It is a long established fact that a reader will be distracted by the\r\n \r\n"
val input2 =
  "It is a long established fact that a reader will be distracted by the"

//horizontal rule
//captures horizontal rule preceded by a non indented space (that is, up to 3 spaces)
val input3 = "   ____\r\n" // captures
PrettyPrint1(new BlockParser1(input3))

//atx heading
val input4 = "### Test your header\r\n" // captures
PrettyPrint1(new BlockParser1(input4))

//block quote
val input5 = "> Hello this is block quote\r\n> this is continuation of a block quote\r\n\r\nthis is a plaint text"
PrettyPrint1(new BlockParser1(input5))