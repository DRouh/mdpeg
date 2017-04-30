import com.mdpeg.{ListBlockParser, Markdown, OrderedList, UnorderedList}
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

  def hasSucceededUnordered(parserResult: Try[UnorderedList]): Boolean = !hasFailedUnordered(parserResult)
  def hasFailedUnordered(parserResult: Try[UnorderedList]): Boolean = parserResult match {
    case Failure(_) => true
    case Success(_) => false
  }
  def hasFailedOrdered(parsed: Try[OrderedList]) = parsed match {
    case Failure(_) => true
    case Success(_) => false
  }

//  val expectedFirst =
//    s"""${TestData.firstItemInList}
//       |""".stripMargin
//  val expectedSecond =
//    s"""${TestData.secondItemInList}""".stripMargin
//
//  it should "parse unordered list's bullets '-*+'" in {
//    for (ch <- Vector("-","*","+")) {
//      val term = s"""$ch """.stripMargin
//      new ListParserTestSpec(term).bullet.run().get
//    }
//  }
//
//  it should "parse ordered list's enumerator 1..999" in {
//    for (d <- 1 to 999) {
//      val term = s"""$d. """.stripMargin
//      new ListParserTestSpec(term).enumerator.run().get
//    }
//  }
//
//  it should "parse tight bullet list" in {
//    val parsed = new ListParserTestSpec(TestData.tightUnorderedList).list.run()
//    parsed.get shouldEqual UnorderedList(Vector(Vector(Markdown(expectedFirst), Markdown(expectedSecond))))
//    println(parsed)
//  }
//
//  it should "fail on sparse bullet list while parsing it as tight" in {
//    val parsed: Try[UnorderedList] = new ListParserTestSpec(TestData.sparseUnorderedList).bulletListTight.run()
//    hasFailedUnordered(parsed) shouldEqual true
//  }
//
//  it should "parse sparse bullet list" in {
//    val parsed = new ListParserTestSpec(TestData.sparseUnorderedList).list.run()
//    parsed.get shouldEqual UnorderedList(Vector(Vector(Markdown(expectedFirst), Markdown(expectedSecond))))
//  }
//
//  it should "parse tight ordered list" in {
//    val parsed = new ListParserTestSpec(TestData.tightOrderedList).list.run()
//    parsed.get shouldEqual OrderedList(Vector(Vector(Markdown(expectedFirst), Markdown(expectedSecond))))
//    println(parsed.get)
//  }
//
//  it should "parse sparse ordered list" in {
//    val parsed = new ListParserTestSpec(TestData.sparseOrderedList).list.run()
//    parsed.get shouldEqual OrderedList(Vector(Vector(Markdown(expectedFirst), Markdown(expectedSecond))))
//  }
//
//  it should "fail sparse ordered list while parsing it as list" in {
//    val parsed = new ListParserTestSpec(TestData.sparseOrderedList).orderedListTight.run()
//    hasFailedOrdered(parsed) shouldEqual true
//  }
  it should "parse nested bullet list" in {
    val term =
      """- item
        |   - sub
        |   - sub78(^%^%$#@#
        |
        |- 2nd item
        |
        |""".stripMargin
    val parser =  new ListParserTestSpec(term)

    println(parser.list.run().get)
//    parser.unorderedList.run() match {
//      case Success(node) => println(node)
//      case Failure(e: ParseError) =>
//        println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
//    }
  }

  UnorderedList(
    Vector(
      Vector(Markdown("first outer")),
      Vector(Markdown("first nested")),
      Vector(Markdown("second nested")),
      Vector(Markdown("second outer"),Markdown("{{md-break}}"))))
}
