import com.mdpeg.MultilineTablesParser
import com.mdpeg.Parser.parser
import org.parboiled2.{ErrorFormatter, ParseError, Parser, ParserInput}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class MultilineTablesParserSpec extends FlatSpec with Matchers {

  class MultilineTablesParserTestSpec(val input: ParserInput) extends Parser with MultilineTablesParser {
  }

  it should "parse table border" in {
    val parsed = new MultilineTablesParserTestSpec("-------------------------------------------------------------------------------\r\n").tableBorder.run()
    noException should be thrownBy {
      parsed.get
    }
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
      """-----------           --------------------
        |.It 1                 is a long established fact that 1
        |.It 2                 is a long established fact that 2
        |.It 3                 is a long established fact that 3
        |
        |CAPSED WORD 1         The point of using Lorem Ipsum is 1
        |CAPSED WORD 2         The point of using Lorem Ipsum is 2
        |
        |Many                  desktop publishing packages and
        |--------------------------------------------------------
        |""".stripMargin
    val parsed = new MultilineTablesParserTestSpec(term).tableBodyRaw.run()
    noException should be thrownBy { parsed.get }
  }

  it should "parser table with header and caption" in {
    val term =
      """--------------------------------------------------------------------------------
        |Term                  Description
        |----------------      ------------------------------------------------
        |.It                   is a long established fact that
        |
        |CAPSED WORD           The point of using Lorem Ipsum is
        |
        |Many                  desktop publishing packages and
        |--------------------------------------------------------------------------------
        |Table: This is a table caption\label{table:table_lable_name}""".stripMargin
    val parser = new MultilineTablesParserTestSpec(term)
    parser.multiTable.run() match {
      case Success(node) => println(node)
      case Failure(e: ParseError) =>
        println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) =>
        throw e
    }
  }
}
