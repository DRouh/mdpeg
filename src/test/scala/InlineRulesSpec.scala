import com.mdpeg._
import org.parboiled2._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

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

  it should "parse explicit link with title" in {
    val term = "[like this](google.com 'title')"
    val parser = new InlineRulesTestSpec(term)
    parser.link.run().get shouldEqual
      Link(Vector(Text("like"), Space, Text("this")),Src("google.com",Some("title")))
  }

  it should "parse explicit link without title" in {
    val term = "[like this](google.com)"
    val parser = new InlineRulesTestSpec(term)
    parser.link.run().get shouldEqual
      Link(Vector(Text("like"), Space, Text("this")),Src("google.com", None))
  }

  it should "parse reference link" in {
    val term = "[I'm a reference link][Arbitrary reference text]"
    val parser = new InlineRulesTestSpec(term)
    parser.link.run().get shouldEqual
      Link(Vector(Text("I'm"), Space, Text("a"), Space, Text("reference"), Space, Text("link")),
        Ref(Vector(Text("Arbitrary"), Space, Text("reference"), Space, Text("text")),""))
  }

  it should "parse reference link ShortcutRef style" in {
    val term = "[I'm a reference link]"
    val parser = new InlineRulesTestSpec(term)
    parser.link.run().get shouldEqual
      Link(Vector(Text("I'm"), Space, Text("a"), Space, Text("reference"), Space, Text("link")),
        ShortcutRef)
  }

  it should "parser autolink uri" in {
    val term = "<http://google.com>"
    val parser = new InlineRulesTestSpec(term)
    parser.inline.run().get shouldEqual
      Link(Vector(Text("http://google.com")),Src("http://google.com",None))
  }

  it should "parser autolink email" in {
    val term = "<test@gmail.com>"
    val parser = new InlineRulesTestSpec(term)
    parser.inline.run().get shouldEqual
      Link(Vector(Text("test@gmail.com")),Src("mailto:test@gmail.com",None))
  }
}