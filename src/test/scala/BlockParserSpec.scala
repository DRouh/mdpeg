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

  it should "parse a Verbatim with long spaces" in {
    val term = s"""```${TestData.codeBlock2}```""".stripMargin
    val parsed = new BlockParser(term).verbatim.run().get
    parsed shouldEqual Verbatim(TestData.codeBlock2)
  }

  it should "parse a JSON like Verbatim" in {
    val term = s"""```${TestData.codeBlock3}```""".stripMargin
    val parsed = new BlockParser(term).verbatim.run().get
    parsed shouldEqual Verbatim(TestData.codeBlock3)
  }

  it should "parse a Verbatim with blank lines, comments, spaces" in {
    val term = s"""```${TestData.codeBlock4}```""".stripMargin
    val parsed = new BlockParser(term).verbatim.run().get
    parsed shouldEqual Verbatim(TestData.codeBlock4)
  }

  it should "parse a reference with title" in {
    val term = s"""[arbitrary case-insensitive reference text]: https://www.mozilla.org 'this is title'""".stripMargin
    new BlockParser(term).reference.run().get shouldEqual ReferenceBlock("arbitrary case-insensitive reference text", Src("https://www.mozilla.org", Some("this is title")))
  }

  it should "parse a reference without title" in {
    val term = s"""[arbitrary case-insensitive 123 !@#]: https://www.mozilla.org""".stripMargin
    val parser = new BlockParser(term)
    new BlockParser(term).reference.run().get shouldEqual ReferenceBlock("arbitrary case-insensitive 123 !@#", Src("https://www.mozilla.org", None))
  }

  it should "parse several consequent multiline tables split by different intervals" in {
    val term =
      s"""${TestData.complexTable}
         |${TestData.complexTable}
         |
         |${TestData.complexTable}
         |
         |
         |${TestData.complexTable}
         |""".stripMargin
    val parsed = new BlockParser(term).InputLine.run().get
    parsed shouldEqual Vector(
      ExpectedTestResults.complexTable,
      ExpectedTestResults.complexTable,
      ExpectedTestResults.complexTable,
      ExpectedTestResults.complexTable)
  }

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
      Verbatim(TestData.codeBlock4),
      Plain(TestData.plainText),
      ExpectedTestResults.unorderedList,
      ExpectedTestResults.orderedList,
      ExpectedTestResults.complexTable
    )
  }
}
