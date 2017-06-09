import com.mdpeg.ASTTransform._
import com.mdpeg._
import org.scalatest.{FlatSpec, Matchers}

class ASTTransformSpec extends FlatSpec with Matchers {

  it should "parse Markdown blocks to block seq" in {
    val rawAstTree = Vector(
      Markdown("This is quote"),
      Markdown("and should span several"),
      Markdown("yet another line for the block")
    )

    val transformedTree = rawAstTree |> transformTree

    transformedTree shouldEqual
    Right(
      Vector(
        Vector(Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote")))),
        Vector(Plain(Vector(Text("and"), Space, Text("should"), Space, Text("span"), Space, Text("several")))),
        Vector(Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space, Text("the"), Space, Text("block")))))
    )
  }

  it should "process Markdown blocks nested inside other blocks" in {
    val rawAstTree = Vector(
      BlockQuote(
        Vector(
          Markdown("This is quote"),
          Markdown("and should span several"),
          Markdown("yet another line for the block")))
    )
    val transformedTree = rawAstTree |> transformTree

    transformedTree shouldEqual
    Right(
      Vector(
        Vector(
          BlockQuote(
            Vector(
              Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote"))),
              Plain(Vector(Text("and"), Space, Text("should"), Space, Text("span"), Space, Text("several"))),
              Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space, Text("the"), Space, Text("block")))))))
    )
  }

  it should "process multiple nested levels of Markdown blocks nested inside other blocks" in {
    val term =
      s"""> ${TestData.blockQuoteLineOne}
         |> ${TestData.blockQuoteLineTwo}
         |> ${TestData.blockQuoteLineThree}
         |""".stripMargin

    val rawAstTree = Vector(
      BlockQuote(
        Vector(
          Markdown("This is quote"),
          Markdown(term),
          Markdown("yet another line for the block")))
    )
    val transformedTree = rawAstTree |> transformTree

    transformedTree shouldEqual
    Right(
      Vector(
        Vector(
          BlockQuote(
            Vector(
              Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote"))),
              BlockQuote(
                Vector(
                  Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote"))),
                  Plain(Vector(Text("and"), Space, Text("should"), Space, Text("span"), Space, Text("several"))),
                  Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space, Text("the"), Space, Text("block"))))),
              Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space, Text("the"), Space, Text("block")))))))
    )
  }

  it should "parse Multiline Table's body nested elements" in {
    val rawTree = Vector(MultilineTableBlock(Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),None,None,
      Vector(
        Vector(MultilineTableCell(Vector(Markdown(".It is longer")))),
        Vector(MultilineTableCell(Vector(Markdown("than neccesary")))),
        Vector(MultilineTableCell(Vector(Markdown(" and it should")))),
        Vector(MultilineTableCell(Vector(Markdown(" be truncated")))),
        Vector(MultilineTableCell(Vector(Markdown(" :)")))))))
    val transformedTree = rawTree |> transformTree

    transformedTree shouldEqual
      Right(Vector(Vector(MultilineTableBlock(Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),None,None,
        Vector(
          Vector(MultilineTableCell(Vector(MultilineTableCell(Vector(Plain(Vector(Text(".It"), Space, Text("is"), Space, Text("longer")))))))),
          Vector(MultilineTableCell(Vector(MultilineTableCell(Vector(Plain(Vector(Text("than"), Space, Text("neccesary")))))))),
          Vector(MultilineTableCell(Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text("and"), Space, Text("it"), Space, Text("should")))))))),
          Vector(MultilineTableCell(Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text("be"), Space, Text("truncated")))))))),
          Vector(MultilineTableCell(Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text(":)"))))))))
        )))))
  }
}
