import com.mdpeg.{InlineRules, PrimitiveRules, Strong}
import org.parboiled2._
import org.scalatest.{FlatSpec, Matchers}

class InlineRulesSpec extends FlatSpec with Matchers {

  class InlineRulesTestSpec(val input: ParserInput) extends Parser with PrimitiveRules with InlineRules {}

  it should "parse strong starred" in {
    val term = "**strong HI**"
    new InlineRulesTestSpec(term).strong.run().get shouldEqual Strong("strong HI")
  }

  it should "parse strong underlined" in {
    val term = "__strong HI__"
    new InlineRulesTestSpec(term).strong.run().get shouldEqual Strong("strong HI")
  }
}