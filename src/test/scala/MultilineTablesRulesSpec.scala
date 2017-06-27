import com.mdpeg.Parser.{fileText, parser}
import com.mdpeg._
import org.parboiled2.{ErrorFormatter, ParseError, Parser, ParserInput}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class MultilineTablesRulesSpec extends FlatSpec with Matchers {

  class MultilineTablesRulesTestSpec(val input: ParserInput) extends Parser with PrimitiveRules with MultilineTablesRules {
  }

  def tableMock(bodyColumns: Vector[MultilineTableColumn],
                head: Option[MultilineTableRow] = Some(Vector(
                  MultilineTableCell(Vector(Markdown(
                    """Term  1
                      |Term  cont""".stripMargin))),
                  MultilineTableCell(Vector(Markdown(
                    """Description 1
                      |Description cont""".stripMargin))))),
                widths: Vector[Float] = Vector(25.0f, 75.0f)) = MultilineTableBlock(
    widths,
    Some(MultilineTableCaption(Vector(Markdown("This is a table caption")), Some("table:table_lable_name"))),
    head,
    bodyColumns
  )

  it should "parse table border" in {
    val term =
      """-------------------------------------------------------------------------------
        |""".stripMargin
    val parsed = new MultilineTablesRulesTestSpec(term).tableBorder.run()
    noException should be thrownBy {
      parsed.get
    }
  }

  it should "parse table caption" in {
    val term =
      """-------------------------------------------------------------
        |Table: This is a table caption\\label{table:table_lable_name}
        |
        |
        |""".stripMargin
    val parsed = new MultilineTablesRulesTestSpec(term).tableCaption.run()
    parsed.get shouldEqual Some("""This is a table caption\\label{table:table_lable_name}""")
  }

  it should "parse table empty caption" in {
    val term =
      """-------------------------------------------------------------
        |
        |
        |""".stripMargin
    val parsed = new MultilineTablesRulesTestSpec(term).tableCaption.run()
    parsed.get shouldEqual None
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
    val parsed = new MultilineTablesRulesTestSpec(term).tableHeadWidthSeparator.run()
    val parsed2 = new MultilineTablesRulesTestSpec(term2).tableHeadWidthSeparator.run()
    val parsed3 = new MultilineTablesRulesTestSpec(term3).tableHeadWidthSeparator.run()
    noException should be thrownBy {
      parsed.get
    }
    noException should be thrownBy {
      parsed2.get
    }
    noException should be thrownBy {
      parsed3.get
    }
  }

  it should "parse table a one-line tall heading" in {
    val term =
      """--------------------------------------------------------------------------------
        |Term 1                Description line 1
        |----------------      ------------------------------------------------
        |""".stripMargin
    val parsed = new MultilineTablesRulesTestSpec(term).tableHeadRaw.run()
    noException should be thrownBy {
      parsed.get
    }
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
    val parsed = new MultilineTablesRulesTestSpec(term).tableHeadRaw.run()
    noException should be thrownBy {
      parsed.get
    }
  }

  it should "fail parsing table with no content lines" in {
    val term =
      """--------------------------------------------------------------------------------
        |----------------      ------------------------------------------------
        |""".stripMargin
    val parsed = new MultilineTablesRulesTestSpec(term).tableHeadRaw.run()
    a[ParseError] should be thrownBy {
      parsed.get
    }
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
    val parsed = new MultilineTablesRulesTestSpec(term).tableBody.run()
    noException should be thrownBy {
      parsed.get
    }
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
    val parser = new MultilineTablesRulesTestSpec(term)
    parser.multiTable.run().get shouldEqual tableMock(Vector(
      Vector(
        MultilineTableCell(Vector(Markdown(".It"))),
        MultilineTableCell(Vector(Markdown("CAPSED WORD"))),
        MultilineTableCell(Vector(Markdown("Many")))),
      Vector(
        MultilineTableCell(Vector(Markdown("is a long established fact that"))),
        MultilineTableCell(Vector(Markdown("The point of using Lorem Ipsum is"))),
        MultilineTableCell(Vector(Markdown("desktop publishing packages and")))))
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
    val parser = new MultilineTablesRulesTestSpec(term)
    parser.multiTable.run().get shouldEqual tableMock(Vector(
      Vector(
        MultilineTableCell(Vector(Markdown(".It"))),
        MultilineTableCell(Vector(Markdown(
          """CAPSED WORD
            |Many""".stripMargin)))),
      Vector(
        MultilineTableCell(Vector(Markdown("is a long established fact that"))),
        MultilineTableCell(Vector(Markdown(
          """The point of using Lorem Ipsum is
            |desktop publishing packages and""".stripMargin))))))
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
    val parser = new MultilineTablesRulesTestSpec(term)
    parser.multiTable.run().get shouldEqual tableMock(Vector(
      Vector(MultilineTableCell(Vector(Markdown(".It")))),
      Vector(MultilineTableCell(Vector(Markdown("is a long established fact that")))
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
    val parser = new MultilineTablesRulesTestSpec(term)
    parser.multiTable.run().get shouldEqual tableMock(
      Vector(Vector(MultilineTableCell(Vector(Markdown(".It"))))),
      Some(Vector(MultilineTableCell(Vector(Markdown(
        """Term  1
          |Term  cont""".stripMargin))))),
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
    val parser = new MultilineTablesRulesTestSpec(term)
    parser.multiTable.run().get shouldEqual MultilineTableBlock(
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
        Vector(MultilineTableCell(Vector(Markdown("table"))))))
  }

  it should "parse doesn't cut text that doesn't fit into width separator" in {
    val term =
      """-----------   -----------   -----------   -----------  -----------
        |.It is longer than neccesary and it should be truncated :)
        |--------------------------------------------------------------------------------
        |""".stripMargin
    val parser = new MultilineTablesRulesTestSpec(term)
    parser.multiTable.run().get shouldEqual MultilineTableBlock(
      Vector(20.0f, 20.0f, 20.0f, 20.0f, 20.0f),
      None,
      None,
      Vector(
        Vector(MultilineTableCell(Vector(Markdown(".It is longer")))),
        Vector(MultilineTableCell(Vector(Markdown("than neccesary")))),
        Vector(MultilineTableCell(Vector(Markdown(" and it should")))),
        Vector(MultilineTableCell(Vector(Markdown(" be truncated")))),
        Vector(MultilineTableCell(Vector(Markdown(" :)"))))))
  }

  it should "parse table with non equal number of lines in cells" in {
    val term =
      """--------------------------------------------------------------------------------
        |This header is longer than sep    And this header is also longer than this separator
        |-----------                       ---------------------------------
        |**Why do we use it?**
        |
        |There-are                         It is a long established fact that a reader will be
        |                                  distracted by the readable content of a page when looking at
        |
        |**Where can I get some?**
        |
        |dummy                             It uses a dictionary of over
        |                                  Lorem Ipsum which looks reasonable
        |
        |text                              The generated Lorem Ipsum is
        |
        |printing                          or non-characteristic words etc
        |
        |**Where does it come from?**
        |
        |leap-into                         It uses a dictionary of over 200
        |                                  you need to be sure there
        |
        |variations-join                   anything embarrassing hidden
        |                                  you need to be sure there isn't
        |                                  within this period
        |
        |**What is Lorem Ipsum?**
        |
        |Lorem                             "There are many variations of passages.
        |                                  *randomised words which : 1597 z*
        |
        |anything                          but the majority have suffered alteration.
        |                                  *to use a passage: "" (empty string)*
        |--------------------------------------------------------------------------------
        |Table: This is a table caption\label{table:table_lable_name}""".stripMargin
    val parser = new MultilineTablesRulesTestSpec(term)
    parser.multiTable.run().get shouldEqual MultilineTableBlock(
      Vector(25.0f, 75.0f),
      Some(MultilineTableCaption(Vector(Markdown("This is a table caption")), Some("table:table_lable_name"))),
      Some(Vector(
        MultilineTableCell(Vector(Markdown("This header is longer than sep"))),
        MultilineTableCell(Vector(Markdown("And this header is also longer than this separator"))))),
      Vector(
        Vector(
          MultilineTableCell(Vector(Markdown("**Why do we use it?**"))),
          MultilineTableCell(Vector(Markdown(
            """There-are
              |""".stripMargin))),
          MultilineTableCell(Vector(Markdown("**Where can I get some?**"))),
          MultilineTableCell(Vector(Markdown(
            """dummy
              |""".stripMargin))),
          MultilineTableCell(Vector(Markdown("text"))),
          MultilineTableCell(Vector(Markdown("printing"))),
          MultilineTableCell(Vector(Markdown("**Where does it come from?**"))),
          MultilineTableCell(Vector(Markdown(
            """leap-into
              |""".stripMargin))),
          MultilineTableCell(Vector(Markdown(
            """variations-join
              |
              |""".stripMargin))),
          MultilineTableCell(Vector(Markdown("**What is Lorem Ipsum?**"))),
          MultilineTableCell(Vector(Markdown(
            """Lorem
              |""".stripMargin))),
          MultilineTableCell(Vector(Markdown(
            """anything
              |""".stripMargin)))),
        Vector(
          MultilineTableCell(Vector(Markdown(""))),
          MultilineTableCell(Vector(Markdown(
            """It is a long established fact that a reader will be
              |distracted by the readable content of a page when looking at""".stripMargin))),
          MultilineTableCell(Vector(Markdown(""))),
          MultilineTableCell(Vector(Markdown(
            """It uses a dictionary of over
              |Lorem Ipsum which looks reasonable""".stripMargin))),
          MultilineTableCell(Vector(Markdown("The generated Lorem Ipsum is"))),
          MultilineTableCell(Vector(Markdown("or non-characteristic words etc"))),
          MultilineTableCell(Vector(Markdown(""))),
          MultilineTableCell(Vector(Markdown(
            """It uses a dictionary of over 200
              |you need to be sure there""".stripMargin))),
          MultilineTableCell(Vector(Markdown(
            """anything embarrassing hidden
              |you need to be sure there isn't
              |within this period""".stripMargin))),
          MultilineTableCell(Vector(Markdown(""))),
          MultilineTableCell(Vector(Markdown(
            """"There are many variations of passages.
              |*randomised words which : 1597 z*""".stripMargin))),
          MultilineTableCell(Vector(Markdown(
            """but the majority have suffered alteration.
              |*to use a passage: "" (empty string)*""".stripMargin))))))
  }
}