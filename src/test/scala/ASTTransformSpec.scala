package org.mdpeg

import org.mdpeg.ASTTransform._
import org.mdpeg.ast._
import org.mdpeg.parsers.BlockParser
import org.scalatest.{FlatSpec, Matchers}

class ASTTransformSpec extends FlatSpec with Matchers {

  it should "process Markdown blocks to block seq" in {
    val rawAstTree = Vector(
      Markdown(RawMarkdownContent("This is quote")),
      Markdown(RawMarkdownContent("and should span several")),
      Markdown(RawMarkdownContent("yet another line for the block"))
    )

    val transformedTree = rawAstTree |> transformTree

    transformedTree shouldEqual
      Right(Ast(
        Vector(
          Vector(
            Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote")))),
          Vector(
            Plain(Vector(Text("and"), Space, Text("should"), Space, Text("span"), Space, Text("several")))),
          Vector(
            Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space, Text("the"),
              Space, Text("block")))))
      ))
  }

  it should "process Markdown blocks nested inside other blocks" in {
    val rawAstTree = Vector(
      BlockQuote(
        Vector(
          Markdown(RawMarkdownContent("This is quote")),
          Markdown(RawMarkdownContent("and should span several")),
          Markdown(RawMarkdownContent("yet another line for the block"))))
    )
    val transformedTree = rawAstTree |> transformTree

    transformedTree shouldEqual
      Right(Ast(
        Vector(
          Vector(
            BlockQuote(
              Vector(
                Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote"))),
                Plain(Vector(Text("and"), Space, Text("should"), Space, Text("span"), Space, Text("several"))),
                Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space,
                  Text("the"), Space, Text("block")))))))
      ))
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
          Markdown(RawMarkdownContent("This is quote")),
          Markdown(RawMarkdownContent(term)),
          Markdown(RawMarkdownContent("yet another line for the block"))))
    )
    val transformedTree = rawAstTree |> transformTree

    transformedTree shouldEqual
      Right(Ast(
        Vector(
          Vector(
            BlockQuote(
              Vector(
                Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote"))),
                BlockQuote(
                  Vector(
                    Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote"))),
                    Plain(Vector(Text("and"), Space, Text("should"), Space, Text("span"), Space, Text("several"))),
                    Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space,
                      Text("the"), Space, Text("block"))))),
                Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space,
                  Text("the"), Space, Text("block")))))))
      ))
  }

  it should "process Multiline Table's body nested elements" in {
    val rawTree = Vector(MultilineTableBlock(Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f), None, None,
      Vector(
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent(".It is longer"))))),
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent("than neccesary"))))),
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent(" and it should"))))),
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent(" be truncated"))))),
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent(" :)"))))))))
    val transformedTree = rawTree |> transformTree

    transformedTree shouldEqual
      Right(Ast(
        Vector(Vector(MultilineTableBlock(Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f), None, None,
        Vector(
          Vector(MultilineTableCell(Vector(Plain(Vector(Text(".It"), Space, Text("is"), Space, Text("longer")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Text("than"), Space, Text("neccesary")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text("and"), Space, Text("it"), Space, Text("should")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text("be"), Space, Text("truncated")))))),
          Vector(MultilineTableCell(Vector(Plain(Vector(Space, Text(":)"))))))
        ))))))
  }

  it should "process nested elements in a rectangular Multiline Table" in {
    val rawTree = Vector(MultilineTableBlock(
      Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),
      Some(MultilineTableCaption(Vector(Markdown(RawMarkdownContent("This is a table caption"))), Some("table:table_lable_name"))),
      Some(
        Vector(
          MultilineTableCell(Vector(Markdown(RawMarkdownContent("Term  1")))),
          MultilineTableCell(Vector(Markdown(RawMarkdownContent("Term  2")))),
          MultilineTableCell(Vector(Markdown(RawMarkdownContent("Term  3")))),
          MultilineTableCell(Vector(Markdown(RawMarkdownContent("Term  4")))),
          MultilineTableCell(Vector(Markdown(RawMarkdownContent("Term  5")))))),
      Vector(
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent(".It"))))),
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent("is"))))),
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent(" a"))))),
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent("rectangular"))))),
        Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent("table"))))))))

    val transformedTree = rawTree |> transformTree

    transformedTree shouldEqual
      Right(Ast(
        Vector(Vector(MultilineTableBlock(Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),
        Some(
          MultilineTableCaption(Vector(
            Plain(Vector(Text("This"), Space, Text("is"), Space, Text("a"), Space, Text("table"), Space,
              Text("caption")))),
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
          Vector(MultilineTableCell(Vector(Plain(Vector(Text("table"))))))
        ))))))
  }

  it should "process Multiline Table's with nested elements spanning multiple lines in a cell" in {
    val rawTree = Vector(
      MultilineTableBlock(Vector(25.0f, 75.0f),
        Some(MultilineTableCaption(Vector(Markdown(RawMarkdownContent("This is a table caption"))), Some("table:table_lable_name"))),
        Some(Vector(MultilineTableCell(Vector(Markdown(RawMarkdownContent(
          """Term  1
            |Term  cont""".stripMargin)))),
          MultilineTableCell(Vector(Markdown(RawMarkdownContent(
            """Description 1
              |Description cont""".stripMargin)))))),
        Vector(
          Vector(
            MultilineTableCell(Vector(Markdown(RawMarkdownContent(".It")))),
            MultilineTableCell(Vector(Markdown(RawMarkdownContent(
              """CAPSED WORD
                |Many""".stripMargin))))),
          Vector(
            MultilineTableCell(Vector(Markdown(RawMarkdownContent("is a long established fact that")))),
            MultilineTableCell(Vector(Markdown(RawMarkdownContent(
              """The point of using Lorem Ipsum is
                |desktop publishing packages and""".stripMargin)))))
        )))

    val transformedTree = rawTree |> transformTree

    transformedTree shouldEqual
      Right(Ast(
        Vector(Vector(MultilineTableBlock(Vector(25.0f, 75.0f),
        Some(
          MultilineTableCaption(Vector(
            Plain(Vector(Text("This"), Space, Text("is"), Space, Text("a"), Space, Text("table"), Space,
              Text("caption")))), Some("table:table_lable_name"))),
        Some(Vector(
          MultilineTableCell(Vector(
            Plain(Vector(Text("Term"), Space, Text("1"), Space, Text("Term"), Space, Text("cont"))))),
          MultilineTableCell(Vector(
            Plain(Vector(Text("Description"), Space, Text("1"), Space, Text("Description"), Space, Text("cont"))))))),
        Vector(
          Vector(
            MultilineTableCell(Vector(Plain(Vector(Text(".It"))))),
            MultilineTableCell(Vector(Plain(Vector(Text("CAPSED"), Space, Text("WORD"), Space, Text("Many")))))),
          Vector(
            MultilineTableCell(Vector(Plain(Vector(
              Text("is"), Space, Text("a"), Space, Text("long"), Space, Text("established"), Space, Text("fact"),
              Space, Text("that"))))),
            MultilineTableCell(Vector(Plain(Vector(
              Text("The"), Space, Text("point"), Space, Text("of"), Space, Text("using"),
              Space, Text("Lorem"), Space, Text("Ipsum"), Space, Text("is"), Space, Text("desktop"), Space,
              Text("publishing"), Space, Text("packages"), Space, Text("and"))))))
        ))))))
  }

  it should "process nested elements in an unordered list" in {
    val nulChar = "\u0000"
    val content =
      s"""item 1
         |${nulChar}  - sub 1
         |  - sub 2""".stripMargin
    val rawTree = Vector(UnorderedList(Vector(Markdown(RawMarkdownContent(content)), Markdown(RawMarkdownContent(content)))))
    rawTree |> transformTree shouldEqual
      Right(Ast(Vector(Vector(
        UnorderedList(Vector(
          Plain(Vector(Text("item"), Space, Text("1"))),
          Plain(Vector(Text("sub"), Space, Text("1"))),
          Plain(Vector(Text("sub"), Space, Text("2"))),
          Plain(Vector(Text("item"), Space, Text("1"))),
          Plain(Vector(Text("sub"), Space, Text("1"))),
          Plain(Vector(Text("sub"), Space, Text("2")))
        ))))))
  }

  it should "process nested elements in an ordered list" in {
    val nulChar = "\u0000"
    val content =
      s"""item 1
         |${nulChar}  1. sub 1
         |  77. sub 2""".stripMargin
    val rawTree = Vector(OrderedList(Vector(Markdown(RawMarkdownContent(content)), Markdown(RawMarkdownContent(content)))))
    rawTree |> transformTree shouldEqual
      Right(Ast(
          Vector(Vector(
        OrderedList(Vector(
          Plain(Vector(Text("item"), Space, Text("1"))),
          Plain(Vector(Text("sub"), Space, Text("1"))),
          Plain(Vector(Text("sub"), Space, Text("2"))),
          Plain(Vector(Text("item"), Space, Text("1"))),
          Plain(Vector(Text("sub"), Space, Text("1"))),
          Plain(Vector(Text("sub"), Space, Text("2")))
        ))))))
  }

  it should "extract links" in {
    val term = TestData.compoundMD
    val parser = new BlockParser(term)
    val rawTree = parser.InputLine.run().get

    val tree = rawTree |> transformTree
    val refs: Map[InlineContent, (String, Option[String])] = tree.right.get |> extractLinks
    refs shouldEqual
      Map(
        Vector(Text("arbitrary"), Space, Text("case-insensitive"), Space, Text("123")) ->
          (("https://www.mozilla.org", None)),
        Vector(Text("arbitrary"), Space, Text("case-insensitive"), Space, Text("reference"), Space, Text("text")) ->
          (("https://www.mozilla.org", Some("this is title"))))
  }
}
