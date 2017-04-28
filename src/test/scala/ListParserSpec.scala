import com.mdpeg.ListBlockParser
import org.parboiled2.{ErrorFormatter, ParseError, Parser, ParserInput}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class ListParserSpec extends FlatSpec with Matchers {

  class ListParserTestSpec(val input: ParserInput) extends Parser with ListBlockParser {}

  it should "parse unordered list's bullets '-*+' " in {
    for (ch <- Vector("-","*","+")) {
      val term = s"""$ch """.stripMargin
      val parser = new ListParserTestSpec(term)
      val parsed = parser.bullet.run().get
    }
  }

  it should "parse ordered list's enumerator 1..999 " in {
    for (d <- 1 to 999) {
      val term = s"""$d. """.stripMargin
      val parser = new ListParserTestSpec(term)
      val parsed = parser.enumerator.run().get
    }
  }
}
