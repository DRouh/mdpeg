import com.mdpeg.{InlineRules, PrimitiveRules}
import org.parboiled2.{Parser, ParserInput}
import org.scalatest.{FlatSpec, Matchers}

class InlineRulesSpec extends FlatSpec with Matchers{
  class InlineRulesTestSpec(val input: ParserInput) extends Parser with PrimitiveRules with InlineRules {}


}