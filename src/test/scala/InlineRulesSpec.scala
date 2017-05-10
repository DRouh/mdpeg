import com.mdpeg._
import org.parboiled2._
import org.scalatest.{FlatSpec, Matchers}

class InlineRulesSpec extends FlatSpec with Matchers {

  class InlineRulesTestSpec(val input: ParserInput) extends Parser with PrimitiveRules with InlineRules {}

  it should "parse strong ** wrapped" in {
    val term = "**strong HI**"
    new InlineRulesTestSpec(term).strong.run().get shouldEqual
      Strong(Vector(Text("strong"), Space, Text("HI")))
  }

  it should "parse strong __ wrapped" in {
    val term = "__strong HI__"
    new InlineRulesTestSpec(term).strong.run().get shouldEqual
      Strong(Vector(Text("strong"), Space, Text("HI")))
  }

  it should "parse italics * wrapped" in {
    val term = "*strong HI*"
    new InlineRulesTestSpec(term).italics.run().get shouldEqual
      Italics(Vector(Text("strong"), Space, Text("HI")))
  }

  it should "parse italics _ wrapped" in {
    val term = "_strong HI_"
    new InlineRulesTestSpec(term).italics.run().get shouldEqual
      Italics(Vector(Text("strong"), Space, Text("HI")))
  }

  it should "parse strong italics *** wrapped" in {
    val term = "***bold italics***"
    new InlineRulesTestSpec(term).inline.run().get shouldEqual
      Strong(Vector(Italics(Vector(Text("bold"), Space, Text("italics")))))
  }

  it should "parse italics strong *__ wrapped" in {
    val term = "*__bold italics__*"
    new InlineRulesTestSpec(term).inline.run().get shouldEqual
      Italics(Vector(Strong(Vector(Text("bold"), Space, Text("italics")))))
  }

  it should "forbid having double strong __** wrapped" in {
    val term = "__**double strong**__"
    a [ParseError] should be thrownBy { new InlineRulesTestSpec(term).inline.run().get }
  }

  it should "forbid having double strong ____ wrapped" in {
    val term = "____double strong____"
    a [ParseError] should be thrownBy { new InlineRulesTestSpec(term).inline.run().get }
  }

  it should "forbid having double strong **** wrapped" in {
    val term = "****double strong****"
    a [ParseError] should be thrownBy { new InlineRulesTestSpec(term).inline.run().get }
  }

  it should "forbid having double italics *_ wrapped" in {
    val term = "*_double strong*_"
    a [ParseError] should be thrownBy { new InlineRulesTestSpec(term).inline.run().get }
  }

  it should "forbid having double italics _* wrapped" in {
    val term = "_*double strong*_"
    a [ParseError] should be thrownBy { new InlineRulesTestSpec(term).inline.run().get }
  }
}