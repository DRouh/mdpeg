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
      lev.times("#") ~ (!endLine ~ !"#" ~ sp ~ capture(inline.+)) ~ anyOf("# \t").* ~ nl.* ~> (HeadingBlock(lev, _))
    }
    rule { h(6) | h(5) | h(4) | h(3) | h(2) | h(1) }
  }

  def horizontalRule: Rule1[HorizontalRuleBlock.type] = {
    @inline
    def h = (ch: String) => rule { ch ~ spOs ~ ch ~ spOs ~ ch ~ (spOs ~ ch).* ~ spOs ~ nl ~ blankLine.* }
    def toHr = (_: String, _: String) => HorizontalRuleBlock
    rule { nonIndentSpace ~ capture(h("-") | h("*") | h("_")) ~> toHr }
  }

  def paragraph : Rule1[Paragraph] = rule { capture((inline | endLine).+) ~ blankLine.+ ~> Paragraph }
  def plain : Rule1[Plain] = rule { capture(inline.+) ~ blankLine.? ~> Plain }
}

val term =
  """# Heading One
    |
    |## Heading Two
    |
    |It is a long established fact that a reader will be distracted by the readable content
    |of a page when looking at its layout. The point of using Lorem Ipsum is that it has a
    |more-or-less normal distribution of letters, as opposed to using, 'Content content'
    |making it look like readable English. Many desktop publishing packages and web page editors
    |now use Lorem Ipsum as their default model text, and a search for will uncover
    |many web sites still in their infancy. Various versions have evolved over the years,
    |sometimes by accident, sometimes on purpose (injected humour and the like).
    |
    |But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born
    |and I will give you a complete account of the system, and expound the actual teachings of the great
    |explorer of the truth, the master-builder of human happiness. No one rejects, dislikes, or avoids
    |pleasure itself, because it is pleasure, but because those who do not know how to pursue pleasure
    |rationally encounter consequences that are extremely painful. Nor again is there anyone who loves
    |or pursues or desires to obtain pain of itself, because it is pain, but because occasionally
    |circumstances occur in which toil and pain can procure him some great pleasure.
    |To take a trivial example, which of
    |
    |****
    |
    |-----
    |
    |> This is quote
    |> and should span several
    |> lines
    |
    |_ _ _ _
    |
    |This is a plaint string in the end.""".stripMargin

PrettyPrint1(new BlockParser1(term))