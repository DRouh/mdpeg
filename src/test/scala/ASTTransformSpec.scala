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

  it should "parse Multiline Table's nested elements" in {
    val rawTree = Vector(MultilineTableBlock(
      Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),
      //Some(MultilineTableCaption(Vector(Markdown("This is a table caption\\label{table:table_lable_name}")))),
      None,
      Some(
        Vector(
          MultilineTableCell(Vector(Markdown("Term  1"))),
          MultilineTableCell(Vector(Markdown("Term  2"))),
          MultilineTableCell(Vector(Markdown("Term  3"))),
          MultilineTableCell(Vector(Markdown("Term  4"))),
          MultilineTableCell(Vector(Markdown("Term  5"))))),
      Vector(
        Vector(MultilineTableCell(Vector(Markdown(".It")))),
        Vector(MultilineTableCell(Vector(Markdown("is")))),
        Vector(MultilineTableCell(Vector(Markdown(" a")))),
        Vector(MultilineTableCell(Vector(Markdown("rectangular")))),
        Vector(MultilineTableCell(Vector(Markdown("table")))))))

    val transformedTree = rawTree |> transformTree

    transformedTree shouldEqual
      Right(Vector(Vector(MultilineTableBlock(Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),None,
        Some(Vector(
              MultilineTableCell(Vector(Plain(Vector(Text("Term"), Space, Text("1"))))),
              MultilineTableCell(Vector(Plain(Vector(Text("Term"), Space, Text("2"))))),
              MultilineTableCell(Vector(Plain(Vector(Text("Term"), Space, Text("3"))))),
              MultilineTableCell(Vector(Plain(Vector(Text("Term"), Space, Text("4"))))),
              MultilineTableCell(Vector(Plain(Vector(Text("Term"), Space, Text("5")))))
            )),
        Vector(
          Vector(MultilineTableCell(Vector(Plain(Vector(Text(".It")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Text("is")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text("a")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Text("rectangular")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Text("table"      ))))))
      )))))
  }
}
