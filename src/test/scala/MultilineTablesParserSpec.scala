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
        |Term                  Description
        |----------------      ------------------------------------------------
        |""".stripMargin
    //     val parsed = new MultilineTablesParserTestSpec(term).tableHead.run()
    //     noException should be thrownBy { parsed.get }
    new MultilineTablesParserTestSpec(term).tableHead.run() match {
      case Success(node) => println(node)
      case Failure(e: ParseError) => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }
}
