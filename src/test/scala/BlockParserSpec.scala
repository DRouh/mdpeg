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
      """> Hello this is block quote
        |> this is continuation of a block quote
        |> yet another line for the block""".stripMargin

    val expectedText =
      """Hello this is block quote
        |this is continuation of a block quote
        |yet another line for the block""".stripMargin
    val parsed = new BlockParser(term).blockQuote.run().get
    parsed shouldEqual BlockQuote(expectedText)
  }

  it should "parse Paragraph" in {
    val term =
      """It is a long established fact that a reader will be distracted by the
        |
        |
        """.stripMargin
    val expected = "It is a long established fact that a reader will be distracted by the"
    val parsed = new BlockParser(term).paragraph.run().get
    parsed shouldEqual Paragraph(expected)
  }

  it should "parse Plain text" in {
    val term = "It is a long established fact that a reader will be distracted by the"
    val expected = "It is a long established fact that a reader will be distracted by the"
    val parsed = new BlockParser(term).plain.run().get
    parsed shouldEqual Plain(expected)
  }
}
