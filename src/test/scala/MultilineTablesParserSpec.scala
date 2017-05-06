import com.mdpeg._
import org.parboiled2.{ParseError, Parser, ParserInput}
import org.scalatest.{FlatSpec, Matchers}

class MultilineTablesParserSpec extends FlatSpec with Matchers {

  class MultilineTablesParserTestSpec(val input: ParserInput) extends Parser with MultilineTablesParser {
  }

  def tableMock(bodyColumns: Vector[MultilineTableColumn],
                head: Option[MultilineTableRow] = Some(Vector(
                  MultilineTableCell(Markdown(
                    """Term  1
                      |Term  cont""".stripMargin)),
                  MultilineTableCell(Markdown(
                    """Description 1
                      |Description cont""".stripMargin)))),
                widths: Vector[Float] = Vector(25.0f, 75.0f)) = MultilineTableBlock(
    widths,
    Some(MultilineTableCaption(Markdown("This is a table caption\\label{table:table_lable_name}"))),
    head,
    bodyColumns
  )

  it should "parse table border" in {
    val term ="""-------------------------------------------------------------------------------
      |""".stripMargin
    val parsed = new MultilineTablesParserTestSpec(term).tableBorder.run()
    noException should be thrownBy { parsed.get }
  }

  it should "parse table caption" in {
    val term = """Table: This is a table caption\\label{table:table_lable_name}"""
    val parsed = new MultilineTablesParserTestSpec(term).tableCaption.run()
    noException should be thrownBy { parsed.get }
  }

  it should "parse table's width separator" in {
    val term =
        """-----------                       ---------------------------------
          |""".stripMargin
    val term2 =
      """-----------  ---  --- --- --- --- ---   -------  ------ ---------------------------------
        |""".stripMargin
    val term3 =
      """-----------
        |""".stripMargin
    val parsed = new MultilineTablesParserTestSpec(term).tableHeadWidthSeparator.run()
    val parsed2 = new MultilineTablesParserTestSpec(term2).tableHeadWidthSeparator.run()
    val parsed3 = new MultilineTablesParserTestSpec(term3).tableHeadWidthSeparator.run()
    noException should be thrownBy { parsed.get }
    noException should be thrownBy { parsed2.get }
    noException should be thrownBy { parsed3.get }
  }

  it should "parse table a one-line tall heading" in {
    val term =
      """--------------------------------------------------------------------------------
        |Term 1                Description line 1
        |----------------      ------------------------------------------------
        |""".stripMargin
    val parsed = new MultilineTablesParserTestSpec(term).tableHeadRaw.run()
    noException should be thrownBy { parsed.get }
  }

  it should "parse table a multi-line tall heading with blank lines" in {
    val term =
      """--------------------------------------------------------------------------------
        |Term 1                Description line 1
        |Term 2                Description line 2
        |
        |Term 3                Description line 3
        |
        |Term 4                Description line 4
        |----------------      ------------------------------------------------
        |""".stripMargin
    val parsed = new MultilineTablesParserTestSpec(term).tableHeadRaw.run()
    noException should be thrownBy { parsed.get }
  }

  it should "fail parsing table with no content lines" in {
    val term =
      """--------------------------------------------------------------------------------
        |----------------      ------------------------------------------------
        |""".stripMargin
    val parsed = new MultilineTablesParserTestSpec(term).tableHeadRaw.run()
    a [ParseError] should be thrownBy { parsed.get }
  }

  it should "parse table content for table without head" in {
    val term =
      """----------------      ------------------------------------------------
        |.It 1                 is a long established fact that 1
        |.It 2                 is a long established fact that 2
        |.It 3                 is a long established fact that 3
        |
        |
        |CAPSED WORD 1         The point of using Lorem Ipsum is 1
        |CAPSED WORD 2         The point of using Lorem Ipsum is 2
        |
        |Many                  desktop publishing packages and""".stripMargin
    val parsed = new MultilineTablesParserTestSpec(term).tableBody.run()
    noException should be thrownBy { parsed.get }
  }

  it should "parse table with header and caption" in {
    val term =
      """--------------------------------------------------------------------------------
        |Term  1               Description 1
        |
        |Term  cont            Description cont
        |----------------      ------------------------------------------------
        |.It                   is a long established fact that
        |
        |CAPSED WORD           The point of using Lorem Ipsum is
        |
        |Many                  desktop publishing packages and
        |--------------------------------------------------------------------------------
        |Table: This is a table caption\label{table:table_lable_name}""".stripMargin
    val parser = new MultilineTablesParserTestSpec(term)
    parser.multiTable.run().get shouldEqual tableMock(Vector(
      Vector(
        MultilineTableCell(Markdown(".It")),
        MultilineTableCell(Markdown("CAPSED WORD")),
        MultilineTableCell(Markdown("Many"))),
      Vector(
        MultilineTableCell(Markdown("is a long established fact that")),
        MultilineTableCell(Markdown("The point of using Lorem Ipsum is")),
        MultilineTableCell(Markdown("desktop publishing packages and"))))
      )
  }

  it should "separate table rows by blank line" in {
    val term =
      """--------------------------------------------------------------------------------
        |Term  1               Description 1
        |
        |Term  cont            Description cont
        |----------------      ------------------------------------------------
        |.It                   is a long established fact that
        |
        |CAPSED WORD           The point of using Lorem Ipsum is
        |Many                  desktop publishing packages and
        |--------------------------------------------------------------------------------
        |Table: This is a table caption\label{table:table_lable_name}""".stripMargin
    val parser = new MultilineTablesParserTestSpec(term)
    parser.multiTable.run().get shouldEqual tableMock(Vector(
      Vector(
        MultilineTableCell(Markdown(".It")),
        MultilineTableCell(Markdown(
          """CAPSED WORD
            |Many""".stripMargin))),
      Vector(
        MultilineTableCell(Markdown("is a long established fact that")),
        MultilineTableCell(Markdown("""The point of using Lorem Ipsum is
                                      |desktop publishing packages and""".stripMargin)))))
  }

  it should "eliminate trailing empty line in body row" in {
    val term =
      """--------------------------------------------------------------------------------
        |Term  1               Description 1
        |
        |Term  cont            Description cont
        |----------------      ------------------------------------------------
        |.It                   is a long established fact that
        |
        |
        |
        |--------------------------------------------------------------------------------
        |Table: This is a table caption\label{table:table_lable_name}""".stripMargin
    val parser = new MultilineTablesParserTestSpec(term)
    parser.multiTable.run().get shouldEqual tableMock(Vector(
      Vector(MultilineTableCell(Markdown(".It"))),
      Vector(MultilineTableCell(Markdown("is a long established fact that"))
    )))
  }

  it should "parse 1x1 table with header" in {
    val term =
      """--------------------------------------------------------------------------------
        |Term  1
        |
        |Term  cont
        |----------------
        |.It
        |
        |--------------------------------------------------------------------------------
        |Table: This is a table caption\label{table:table_lable_name}""".stripMargin
    val parser = new MultilineTablesParserTestSpec(term)
    parser.multiTable.run().get shouldEqual tableMock(
      Vector(Vector(MultilineTableCell(Markdown(".It")))),
      Some(Vector(MultilineTableCell(Markdown("""Term  1
                                                |Term  cont""".stripMargin)))),
      Vector(100.0f))
  }

  it should "parse a rectangular table" in {
    val term =
      """--------------------------------------------------------------------------------
        |Term  1       Term  2       Term  3       Term  4      Term  5
        |-----------   -----------   -----------   -----------  -----------
        |.It           is             a            rectangular  table
        |--------------------------------------------------------------------------------
        |Table: This is a table caption\label{table:table_lable_name}""".stripMargin
    val parser = new MultilineTablesParserTestSpec(term)
    parser.multiTable.run().get shouldEqual MultilineTableBlock(
      Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),
      Some(MultilineTableCaption(Markdown("This is a table caption\\label{table:table_lable_name}"))),
      Some(
        Vector(
          MultilineTableCell(Markdown("Term  1")),
          MultilineTableCell(Markdown("Term  2")),
          MultilineTableCell(Markdown("Term  3")),
          MultilineTableCell(Markdown("Term  4")),
          MultilineTableCell(Markdown("Term  5")))),
      Vector(
        Vector(MultilineTableCell(Markdown(".It"))),
        Vector(MultilineTableCell(Markdown("is"))),
        Vector(MultilineTableCell(Markdown("a"))),
        Vector(MultilineTableCell(Markdown("rectangular"))),
        Vector(MultilineTableCell(Markdown("table")))))
  }

  it should "parse cut all text that doesn't fit into column" in {
    val term =
      """-----------   -----------   -----------   -----------  -----------
        |.It is longer than neccesary and it should be truncated :)
        |--------------------------------------------------------------------------------
        |""".stripMargin
    val parser = new MultilineTablesParserTestSpec(term)
    parser.multiTable.run().get shouldEqual MultilineTableBlock(
      Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),
      None,
      None,
      Vector(
        Vector(MultilineTableCell(Markdown(".It is long"))),
        Vector(MultilineTableCell(Markdown("than necces"))),
        Vector(MultilineTableCell(Markdown("and it sho"))),
        Vector(MultilineTableCell(Markdown("be truncat"))),
        Vector(MultilineTableCell(Markdown(" :)")))))
  }
}