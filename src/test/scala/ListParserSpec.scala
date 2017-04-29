import com.mdpeg.{ListBlockParser, Markdown, UnorderedList}
import org.parboiled2.{ErrorFormatter, ParseError, Parser, ParserInput}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class ListParserSpec extends FlatSpec with Matchers {
  class ListParserTestSpec(val input: ParserInput) extends Parser with ListBlockParser {}
  object PrettyPrintListParser {
    def apply(parser : ListParserTestSpec) : Unit= {
      val result= parser.bulletListItem.run()
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

  def hasSucceeded (parserResult: Try[UnorderedList] ): Boolean  = !hasFailed(parserResult)
  def hasFailed (parserResult: Try[UnorderedList] ): Boolean =
    parserResult match {
      case Failure(_) => true
      case Success(_) => false
    }

  it should "parse unordered list's bullets '-*+'" in {
    for (ch <- Vector("-","*","+")) {
      val term = s"""$ch """.stripMargin
      new ListParserTestSpec(term).bullet.run().get
    }
  }

  it should "parse ordered list's enumerator 1..999" in {
    for (d <- 1 to 999) {
      val term = s"""$d. """.stripMargin
      new ListParserTestSpec(term).enumerator.run().get
    }
  }

  it should "parse tight bullet list" in {
    val term =
      s"""- First item
         |- Second item
       """.stripMargin
    val parsed = new ListParserTestSpec(term).bulletListTight.run()
    parsed.get shouldEqual UnorderedList(Vector(Vector(Markdown("First item\r\n"), Markdown("Second item\r\n"))))
  }

  it should "should fail on sparse bullet list while parsing it as tight" in {
    val term =
      s"""- First item
         |
         |- Second item
       """.stripMargin
    val parsed: Try[UnorderedList] = new ListParserTestSpec(term).bulletListTight.run()
    hasFailed(parsed) shouldEqual true
  }

  it should "parse sparse bullet list" in {
    val term =
      s"""- First item
         |
         |- Second item
         |
       """.stripMargin
    val parsed = new ListParserTestSpec(term).bulletListSparse.run()
    parsed.get shouldEqual UnorderedList(Vector(Vector(Markdown("First item\r\n"), Markdown("Second item\r\n"))))
  }
}
