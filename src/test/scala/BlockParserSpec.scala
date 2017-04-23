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

  it should "parse ATX Heading" in {
    val term =
      """ Test your header
        |
      """.stripMargin
    val expected = "Test your header"
    val ts = (1 to 6).map(i => (i, "#" * i + term))
    for (t <- ts) {
      val parsed = new BlockParser(t._2).heading.run().get
      parsed shouldEqual HeadingBlock(t._1, expected)
    }
  }

  it should "parse a compound document" in {
    val term = TestData.compoundMD
    val parsed = new BlockParser(term).InputLine.run().get
    val headingOne = "Heading One"
    val headingTwo = "Heading Two"
    val paragraphOne ="""It is a long established fact that a reader will be distracted by the readable content
        |of a page when looking at its layout. The point of using Lorem Ipsum is that it has a
        |more-or-less normal distribution of letters, as opposed to using, 'Content content'
        |making it look like readable English. Many desktop publishing packages and web page editors
        |now use Lorem Ipsum as their default model text, and a search for will uncover
        |many web sites still in their infancy. Various versions have evolved over the years,
        |sometimes by accident, sometimes on purpose (injected humour and the like).""".stripMargin
    val paragraphTwo = """But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born
        |and I will give you a complete account of the system, and expound the actual teachings of the great
        |explorer of the truth, the master-builder of human happiness. No one rejects, dislikes, or avoids
        |pleasure itself, because it is pleasure, but because those who do not know how to pursue pleasure
        |rationally encounter consequences that are extremely painful. Nor again is there anyone who loves
        |or pursues or desires to obtain pain of itself, because it is pain, but because occasionally
        |circumstances occur in which toil and pain can procure him some great pleasure.
        |To take a trivial example, which of""".stripMargin
    val blockQuote ="""This is quote
        |and should span several
        |lines
        |""".stripMargin

    val plainText = "This is a plaint string in the end."
    parsed shouldEqual Vector(
      HeadingBlock(1, headingOne),
      HeadingBlock(2, headingTwo),
      Paragraph(paragraphOne),
      Paragraph(paragraphTwo),
      HorizontalRuleBlock,
      HorizontalRuleBlock,
      BlockQuote(blockQuote),
      HorizontalRuleBlock,
      Plain(plainText))
  }
}
