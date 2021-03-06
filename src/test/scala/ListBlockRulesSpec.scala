package org.mdpeg

import org.mdpeg.ast.{Markdown, OrderedList, RawMarkdownContent, UnorderedList}
import org.mdpeg.parsers.{ListBlockRules, PrimitiveRules}
import org.parboiled2.{ErrorFormatter, ParseError, Parser, ParserInput}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class ListBlockRulesSpec extends FlatSpec with Matchers {
  class ListBlockRulesTestSpec(val input: ParserInput) extends Parser with PrimitiveRules with ListBlockRules {}
  object PrettyPrintListParser {
    def apply(parser : ListBlockRulesTestSpec) : Unit= {
      val result= parser.bulletListItem.run()
      result match {
        case Failure(error) =>
          error match {
            case e : ParseError => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
            case _ => println(error)
          }
        case Success(v) => println(v)
      }
    }
  }

  def hasSucceededUnordered(parserResult: Try[UnorderedList]): Boolean = !hasFailedUnordered(parserResult)
  def hasFailedUnordered(parserResult: Try[UnorderedList]): Boolean = parserResult match {
    case Failure(_) => true
    case Success(_) => false
  }
  def hasFailedOrdered(parsed: Try[OrderedList]):Boolean = parsed match {
    case Failure(_) => true
    case Success(_) => false
  }

  val expectedFirst: String =
    s"""${TestData.firstItemInList}""".stripMargin
  val expectedSecond: String =
    s"""${TestData.secondItemInList}""".stripMargin

  it should "parse unordered list's bullets '-*+'" in {
    for (ch <- Vector("-","*","+")) {
      val term = s"""$ch """.stripMargin
      new ListBlockRulesTestSpec(term).bullet.run().get
    }
  }

  it should "parse ordered list's enumerator 1..999" in {
    for (d <- 1 to 999) {
      val term = s"""$d. """.stripMargin
      new ListBlockRulesTestSpec(term).enumerator.run().get
    }
  }

  it should "parse tight bullet list" in {
    val parsed = new ListBlockRulesTestSpec(TestData.tightUnorderedList).list.run()
    parsed.get shouldEqual UnorderedList(Vector(Markdown(RawMarkdownContent(expectedFirst)), Markdown(RawMarkdownContent(expectedSecond))))
  }

  it should "fail on sparse bullet list while parsing it as tight" in {
    val parsed: Try[UnorderedList] = new ListBlockRulesTestSpec(TestData.sparseUnorderedList).bulletListTight.run()
    hasFailedUnordered(parsed) shouldEqual true
  }

  it should "parse sparse bullet list" in { // ToDo fix rules to get rid of these trailing blank lines
    val parsed = new ListBlockRulesTestSpec(TestData.sparseUnorderedList).list.run()
    parsed.get shouldEqual UnorderedList(Vector(Markdown(RawMarkdownContent("First item")), Markdown(RawMarkdownContent("""Second item""".stripMargin))))
  }

  it should "parse tight ordered list" in {
    val parsed = new ListBlockRulesTestSpec(TestData.tightOrderedList).list.run()
    parsed.get shouldEqual OrderedList(Vector(Markdown(RawMarkdownContent(expectedFirst)), Markdown(RawMarkdownContent(expectedSecond))))
  }

  it should "parse sparse ordered list" in { // ToDo fix rules to get rid of these trailing blank lines
    val parsed = new ListBlockRulesTestSpec(TestData.sparseOrderedList).list.run()
    parsed.get shouldEqual OrderedList(Vector(Markdown(RawMarkdownContent(expectedFirst)), Markdown(RawMarkdownContent("""Second item""".stripMargin))))
  }

  it should "fail sparse ordered list while parsing it as list" in {
    val parsed = new ListBlockRulesTestSpec(TestData.sparseOrderedList).orderedListTight.run()
    hasFailedOrdered(parsed) shouldEqual true
  }

  it should "create markdown for every list item in unordered list" in {
    val term ="""* 1st block - It is a long established fact that a reader will be distracted by the readable content of a
                |  page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less
                |    normal distribution of letters, as opposed to using 'Content here, content here',
                |* 2nd list block - editors now use Lorem Ipsum as their default model text, and a search for
                |     'lorem ipsum' will uncover many web sites still in their infancy. Various versions
                |     injected humour and the like).
                |     There are many variations of passages of Lorem Ipsum available, but the majority have
                |* 3rd list block - If you are going to use a passage of Lorem Ipsum, you need to be
                |* 4th list block - sure there isn't anything embarrassing hidden in the middle
                |    of text. All the Lorem Ipsum generators on the Internet tend to r""".stripMargin
    val parser = new ListBlockRulesTestSpec(term)
    parser.list.run().get shouldEqual UnorderedList(
      Vector(
        Markdown(RawMarkdownContent(s"""1st block - It is a long established fact that a reader will be distracted by the readable content of a
                   |  page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less
                   |    normal distribution of letters, as opposed to using 'Content here, content here',""".stripMargin)),
        Markdown(RawMarkdownContent(s"""2nd list block - editors now use Lorem Ipsum as their default model text, and a search for
                   |     'lorem ipsum' will uncover many web sites still in their infancy. Various versions
                   |     injected humour and the like).
                   |     There are many variations of passages of Lorem Ipsum available, but the majority have""".stripMargin)),
        Markdown(RawMarkdownContent("""3rd list block - If you are going to use a passage of Lorem Ipsum, you need to be""".stripMargin)),
        Markdown(RawMarkdownContent(s"""4th list block - sure there isn't anything embarrassing hidden in the middle
                   |    of text. All the Lorem Ipsum generators on the Internet tend to r""".stripMargin))))
  }

  it should "create markdown for each full/half indented chunk in unordered list" in {
    val nulChar = "\u0000"
    val term =
      """- item 1
        |    - sub 1
        |    - sub 2
        |- item 2
        |  - sub 3
        |  - sub 4""".stripMargin
    val parser = new ListBlockRulesTestSpec(term)
    parser.list.run().get shouldEqual UnorderedList(
      Vector(
        Markdown(RawMarkdownContent(s"""item 1
                   |${nulChar}    - sub 1
                   |    - sub 2""".stripMargin)),
        Markdown(RawMarkdownContent(s"""item 2
                   |${nulChar}  - sub 3
                   |  - sub 4""".stripMargin))))
  }

  it should "create markdown for each full/half indented chunk in ordered list" in {
    val nulChar = "\u0000"
    val term =
        """1. item 1
          |     1. sub 1
          |     2. sub 2
          |2. item 2
          |  1. sub 3
          |  2. sub 4""".stripMargin
      val parser = new ListBlockRulesTestSpec(term)
      parser.list.run().get shouldEqual OrderedList(
        Vector(
          Markdown(RawMarkdownContent(s"""item 1
                     |${nulChar}     1. sub 1
                     |     2. sub 2""".stripMargin)),
          Markdown(RawMarkdownContent(s"""item 2
                     |${nulChar}  1. sub 3
                     |  2. sub 4""".stripMargin))))
  }
}
