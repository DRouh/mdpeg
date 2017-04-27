import com.mdpeg.ListBlockParser
import org.parboiled2.{ErrorFormatter, ParseError, Parser, ParserInput}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class ListParserSpec extends FlatSpec with Matchers {

  class ListParserTestSpec(val input: ParserInput) extends Parser with ListBlockParser {}


  it should "parse a list" in {
    val term = s"""- item 1""".stripMargin
    val parser = new ListParserTestSpec(term)
//    val parsed = parser.listBlock.run()
//    parsed match {
//      case Failure(error) => error match {
//        case e: ParseError => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
//        case _ => println(error)
//      }
//      case Success(value) => println(value)
//    }
  }
}
