import com.mdpeg.MultilineTablesParser
import org.parboiled2.{Parser, ParserInput}
import org.scalatest.{FlatSpec, Matchers}

class MultilineTablesParserSpec extends FlatSpec with Matchers  {
  class MultilineTablesParserTestSpec(val input: ParserInput) extends Parser with MultilineTablesParser {}

  it should "parse table border" in {
    val parsed = new MultilineTablesParserTestSpec("-------------------------------------------------------------------------------\r\n").tableBorder.run()
    noException should be thrownBy { parsed.get }
  }
}
