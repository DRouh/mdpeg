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

class BlockParser1(val input: ParserInput) extends PrimitiveRules {
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

  def horizontalRule: Rule1[HorizontalRuleBlock.type] = {
    @inline
    def h = (ch: String) => rule { ch ~ spOs ~ ch ~ spOs ~ ch ~ (spOs ~ ch).* ~ spOs ~ nl ~ blankLine.+ }
    def toHr = (x: String, y: String) => HorizontalRuleBlock
    rule { nonIndentSpace ~ capture(h("-") | h("*") | h("_")) ~> toHr }
  }

  def paragraph : Rule1[Paragraph] = rule { capture(inline.+) ~ nl ~ blankLine.+ ~> Paragraph }
  def plain : Rule1[Plain] = rule { capture(inline.+) ~ blankLine.? ~> Plain }
}

//tests
val input1 =
  """It is a long established fact that a reader will be distracted by the
    |
    |
  """.stripMargin
PrettyPrint1(new BlockParser1(input1)) //paragraph

val input2 = "It is a long established fact that a reader will be distracted by the"
PrettyPrint1(new BlockParser1(input2)) // plain

//atx heading
val input4 = "### Test your header\r\n" // captures
PrettyPrint1(new BlockParser1(input4))

//block quote
val input5 = """> Hello this is block quote
               |> this is continuation of a block quote
               |
               |this is a plaint text""".stripMargin
PrettyPrint1(new BlockParser1(input5))