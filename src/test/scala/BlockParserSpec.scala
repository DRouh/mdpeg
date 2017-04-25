import com.mdpeg._
import org.scalatest.{FlatSpec, Matchers}

class BlockParserSpec extends FlatSpec with Matchers {
  it should "parse '_' Horizontal rule" in {
    val term =
      """   ____
        |
        |""".stripMargin
    val parsed = new BlockParser(term).horizontalRule.run().get
    parsed shouldEqual HorizontalRuleBlock
  }

  it should "parse '-' sparse Horizontal rule" in {
    val term =
      """   - - - -
        |
        |""".stripMargin
    val parsed = new BlockParser(term).horizontalRule.run().get
    parsed shouldEqual HorizontalRuleBlock
  }

  it should "parse '*'-many Horizontal rule" in {
    val term =
      """   ***************
        |
        |""".stripMargin
    val parsed = new BlockParser(term).horizontalRule.run().get
    parsed shouldEqual HorizontalRuleBlock
  }

  it should "parse '>' Block Quote" in {
    val term =
      s"""> ${TestData.blockQuoteLineOne}
        |> ${TestData.blockQuoteLineTwo}
        |> ${TestData.blockQuoteLineThree}
        |""".stripMargin

    val expectedText = TestData.blockQuote
    val parsed = new BlockParser(term).blockQuote.run().get
    parsed shouldEqual BlockQuote(expectedText)
  }

  it should "parse Paragraph" in {
    val term =
      s"""${TestData.paragraphOne}
        |
        |
        """.stripMargin
    val parsed = new BlockParser(term).paragraph.run().get
    parsed shouldEqual Paragraph(TestData.paragraphOne)
  }

  it should "parse Plain text" in {
    val term = TestData.plainText
    val parsed = new BlockParser(term).plain.run().get
    parsed shouldEqual Plain(TestData.plainText)
  }

  it should "parse ATX Heading" in {
    val term =
      s""" ${TestData.headingOne}
        |
      """.stripMargin
    val ts = (1 to 6).map(i => (i, "#" * i + term))
    for (t <- ts) {
      val parsed = new BlockParser(t._2).heading.run().get
      parsed shouldEqual HeadingBlock(t._1, TestData.headingOne)
    }
  }

  it should "parse a Verbatim" in {
    val term = s"""```${TestData.codeBlock}```""".stripMargin
    val parsed = new BlockParser(term).verbatim.run().get
    parsed shouldEqual Verbatim(TestData.codeBlock)
  }
  // ToDo add test for blank lines in a Verbatim block
  // ToDo update compound test upon Verbatim rule parser completion
  it should "parse a compound document" in {
    val term = TestData.compoundMD
    val parsed = new BlockParser(term).InputLine.run().get
    parsed shouldEqual Vector(
      HeadingBlock(1, TestData.headingOne),
      HeadingBlock(2, TestData.headingTwo),
      Paragraph(TestData.paragraphOne),
      Paragraph(TestData.paragraphTwo),
      HorizontalRuleBlock,
      HorizontalRuleBlock,
      BlockQuote(TestData.blockQuote),
      HorizontalRuleBlock,
      Verbatim(TestData.codeBlock),
      Plain(TestData.plainText)
    )
  }
}
