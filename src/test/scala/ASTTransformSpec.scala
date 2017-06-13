import com.mdpeg.ASTTransform._
import com.mdpeg._
import org.scalatest.{FlatSpec, Matchers}

class ASTTransformSpec extends FlatSpec with Matchers {

  it should "process Markdown blocks to block seq" in {
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

  it should "process Multiline Table's body nested elements" in {
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
          Vector(MultilineTableCell(Vector(Plain(Vector(Text(".It"), Space, Text("is"), Space, Text("longer")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Text("than"), Space, Text("neccesary")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text("and"), Space, Text("it"), Space, Text("should")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text("be"), Space, Text("truncated")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text(":)"))))))
        )))))
  }

  it should "process nested elements in a rectangular Multiline Table" in {
    val rawTree = Vector(MultilineTableBlock(
      Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),
      Some(MultilineTableCaption(Vector(Markdown("This is a table caption")), Some("table:table_lable_name"))),
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
      Right(Vector(Vector(MultilineTableBlock(Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),
        Some(
          MultilineTableCaption(Vector(Plain(Vector(Text("This"), Space, Text("is"), Space, Text("a"), Space, Text("table"), Space, Text("caption")))),
          Some("table:table_lable_name"))),
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

  it should "process Multiline Table's with nested elements spanning multiple lines in a cell" in {
    val rawTree = Vector(
      MultilineTableBlock(Vector(25.0f, 75.0f),
      Some(MultilineTableCaption(Vector(Markdown("This is a table caption")),Some("table:table_lable_name"))),
        Some(Vector(MultilineTableCell(Vector(Markdown(
          """Term  1
            |Term  cont""".stripMargin))),
          MultilineTableCell(Vector(Markdown(
            """Description 1
              |Description cont""".stripMargin))))),
        Vector(
          Vector(
            MultilineTableCell(Vector(Markdown(".It"))),
            MultilineTableCell(Vector(Markdown(
              """CAPSED WORD
                |Many""".stripMargin)))),
          Vector(
            MultilineTableCell(Vector(Markdown("is a long established fact that"))),
            MultilineTableCell(Vector(Markdown(
              """The point of using Lorem Ipsum is
                |desktop publishing packages and""".stripMargin))))
        )))

    val transformedTree = rawTree |> transformTree

    transformedTree shouldEqual
      Right(Vector(Vector(MultilineTableBlock(Vector(25.0f, 75.0f),
        Some(MultilineTableCaption(Vector(Plain(Vector(Text("This"), Space, Text("is"), Space, Text("a"), Space, Text("table"), Space, Text("caption")))),Some("table:table_lable_name"))),
        Some(Vector(
          MultilineTableCell(Vector(Plain(Vector(Text("Term"), Space, Text("1"), Space, Text("Term"), Space, Text("cont"))))),
          MultilineTableCell(Vector(Plain(Vector(Text("Description"), Space, Text("1"), Space, Text("Description"), Space, Text("cont"))))))),
        Vector(
          Vector(
            MultilineTableCell(Vector(Plain(Vector(Text(".It"))))),
            MultilineTableCell(Vector(Plain(Vector(Text("CAPSED"), Space, Text("WORD"), Space, Text("Many")))))),
          Vector(
            MultilineTableCell(Vector(Plain(Vector(Text("is"), Space, Text("a"), Space, Text("long"), Space, Text("established"), Space, Text("fact"), Space, Text("that"))))),
            MultilineTableCell(Vector(Plain(Vector(Text("The"), Space, Text("point"), Space, Text("of"), Space, Text("using"), Space, Text("Lorem"), Space, Text("Ipsum"), Space, Text("is"), Space, Text("desktop"), Space, Text("publishing"), Space, Text("packages"), Space, Text("and"))))))
        )))))
  }

  it should "process nested elements in an unordered list" in {
    val rawTree = Vector(UnorderedList(
      Vector(
        Markdown("""item 1
                   |
                   |     - sub 1
                   |     - sub 2
                   |""".stripMargin),
        Markdown("""item 2
                   |
                   |  - sub 3
                   |  - sub 4""".stripMargin))))
    rawTree |> transformTree shouldEqual
      Right(Vector(
        Vector(
          UnorderedList(Vector(
            Plain(Vector(Text("item"), Space, Text("1"))),
            UnorderedList(Vector(
              Plain(Vector(Text("sub"), Space, Text("1"))),
              Plain(Vector(Text("sub"), Space, Text("2"))))
            ),
            Plain(Vector(Text("item"), Space, Text("2"))),
            UnorderedList(Vector(
              Plain(Vector(Text("sub"), Space, Text("3"))),
              Plain(Vector(Text("sub"), Space, Text("4"))))
            )
          )))))
  }

  it should "process nested elements in an unordered list 2" in {
    val nulChar = "\0"

    val rawTree = Vector(UnorderedList(
      Vector(
        Markdown("""* item
                   |    * sub 1
                   |    * sub 2
                   |    * sub 3
                   |    * sub 4""".stripMargin))))


    rawTree |> transformTree shouldEqual
      Right(Vector(Vector(
        Plain(Vector(Text("hello"), Space, Text("from"), Space, Text("the"), Space, Text("other"), Space, Text("side"),
          Space, Text("second"), Space, Text("line"), Space, Text("from"), Space, Text("the"), Space, Text("other"),
          Space, Text("side"), Space, Space))),
        Vector(
          UnorderedList(Vector(
            Plain(Vector(Text("sub"), Space, Text("1"), Space, Text(" "), Space)),
            Plain(Vector(Text("sub"), Space, Text("2"), Space, Text(" "), Space)),
            Plain(Vector(Text("sub"), Space, Text("3"), Space, Text(" "), Space)),
            Plain(Vector(Text("sub"), Space, Text("4")))))
        )))
//    Right(Vector(Vector(
//      UnorderedList(Vector(
//        Plain(Vector(Text("hello"), Space, Text("from"), Space, Text("the"), Space, Text("other"),
//          Space, Text("side"), Space, Text("second"), Space, Text("line"), Space, Text("from"), Space,
//          Text("the"), Space, Text("other"), Space, Text("side"), Space, Space)),
//        UnorderedList(Vector(
//          Plain(Vector(Text("sub"), Space, Text("1"))),
//          Plain(Vector(Space, Text("what"), Space, Text("if"), Space, Text("I"), Space, Text("do"), Space,
//            Text("this"), Space, Space, Text("and"), Space, Text("what"), Space, Text("if"), Space,
//            Text("I"), Space, Text("do"), Space, Text("that"), Space, Space)),
//          UnorderedList(Vector(
//            Plain(Vector(Text("sub"), Space, Text("2"))),
//            Plain(Vector(Space)),
//            UnorderedList(Vector(
//              Plain(Vector(Text("sub"), Space, Text("3"))),
//              Plain(Vector(Space)),
//              UnorderedList(Vector(Plain(Vector(
//                Text("sub"), Space, Text("4")))))
//            )))))))))))

    Right(Vector(Vector(
      UnorderedList(Vector(
        UnorderedList(Vector(
          Plain(Vector(Text("item"))),
          Plain(Vector(Space)),
          UnorderedList(Vector(
            Plain(Vector(Text("sub"), Space, Text("1"))),
            Plain(Vector(Space)),
            UnorderedList(Vector(
              Plain(Vector(Text("sub"), Space, Text("2"))),
              Plain(Vector(Space)),
              UnorderedList(Vector(
                Plain(Vector(Text("sub"), Space, Text("3"))),
                Plain(Vector(Space)),
                UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("4"))))))))))))))))))
  }

  it should "process nested elements in an ordered list" in {
    val rawTree = Vector(UnorderedList(
      Vector(
        Markdown("""item 1
                   |     1. sub 1
                   |     2. sub 2
                   |""".stripMargin),
        Markdown("""item 2
                   |  1. sub 3
                   |  2. sub 4""".stripMargin))))
    rawTree |> transformTree shouldEqual
      Right(Vector(
        Vector(
          UnorderedList(Vector(
            Plain(Vector(Text("item"), Space, Text("1"))),
            UnorderedList(Vector(
              Plain(Vector(Text("sub"), Space, Text("1"))),
              Plain(Vector(Text("sub"), Space, Text("2"))))
            ),
            Plain(Vector(Text("item"), Space, Text("2"))),
            UnorderedList(Vector(
              Plain(Vector(Text("sub"), Space, Text("3"))),
              Plain(Vector(Text("sub"), Space, Text("4"))))
            )
          )))))
  }
}
