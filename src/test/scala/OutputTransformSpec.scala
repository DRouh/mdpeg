import java.io.PrintWriter

import org.mdpeg._
import org.mdpeg.ASTTransform._
import org.mdpeg.OutputTransform._
import org.scalatest.{FlatSpec, Matchers}

class OutputTransformSpec extends FlatSpec with Matchers {

  def process(term: String) = {
    val parser = new BlockParser(term)
    val parsed = parser.InputLine.run()
    val rawAstTree = parsed.get
    rawAstTree |>
      transformTree |>
      (_.right.get) |> { t => (t |> extractLinks |> (_.toMap) |> toHtml) (t) }
  }

  //  it should "transformToHtml" in {
  //   // val term  = TestData.compoundMD//scala.io.Source.fromFile("input.md").mkString
  //    val term  = scala.io.Source.fromFile("input.md").mkString
  //    val parser = new BlockParser(term)
  //    val parsed = parser.InputLine.run()
  //    val rawAstTree = parsed.get
  //    val transformedTree = rawAstTree |> transformTree |> (_.right.get) |> { t => (t |> extractLinks |> (_.toMap) |> toHtml)(t) }
  //
  //    new PrintWriter("output.html") { write(transformedTree.mkString); close }
  //  }

  it should "handle inline-style images" in {
    val term =
      """Inline-style:
        |![Inline-style](https://avatars0.githubusercontent.com/u/1827342?v=3&s=460 "Logo Title Text 1")""".stripMargin
    val actual = term |> process
    actual shouldEqual """Inline-style: <img alt="Inline-style" src="https://avatars0.githubusercontent.com/u/1827342?v=3&s=460" title="Logo Title Text 1"  />"""
  }

  it should "handle reference-style images" in {
    val term =
      """Reference-style:
        |![Reference-style][logo]
        |
        |[logo]: https://avatars0.githubusercontent.com/u/1827342?v=3&s=460 "Logo Title Text 2"""".stripMargin
    val actual = term |> process
    actual shouldEqual
      """<p>Reference-style: <img alt="logo" src="https://avatars0.githubusercontent.com/u/1827342?v=3&s=460" title="Logo Title Text 2"  /></p>
        |""".stripMargin
  }

  it should "handle inline-style links" in {
    val term = """[I'm an inline-style link](https://www.google.com)""".stripMargin
    val actual = term |> process
    actual shouldEqual """<a href="https://www.google.com">I'm an inline-style link</a>"""
  }

  it should "handle reference-style links" in {
    val term =
      """[I'm a reference-style link][Arbitrary case-insensitive reference text]
        |
        |[arbitrary case-insensitive reference text]: https://www.mozilla.org""".stripMargin
    val actual = term |> process
    actual shouldEqual
      """<p><a href="https://www.mozilla.org">I'm a reference-style link</a></p>
        |""".stripMargin
  }

  it should "handle numbers for reference-style link definitions" in {
    val term =
      """[You can use numbers for reference-style link definitions][1]
        |
        |[1]: http://slashdot.org""".stripMargin
    val actual = term |> process
    actual shouldEqual
      """<p><a href="http://slashdot.org">You can use numbers for reference-style link definitions</a></p>
        |""".stripMargin
  }

  it should "handle link text itself for reference-style link definitions" in {
    val term =
      """Or leave it empty and use the [link text itself]
        |
        |[link text itself]: http://www.reddit.com""".stripMargin
    val actual = term |> process
    actual shouldEqual
      """<p>Or leave it empty and use the <a href="http://www.reddit.com">link text itself</a></p>
        |""".stripMargin
  }

  it should "handle auto-links" in {
    val term =
      """URLs and URLs in angle brackets will automatically get turned into links.
        |http://www.example.com or <http://www.example.com> and sometimes
        |example.com (but not on Github, for example).""".stripMargin
    val actual = term |> process
    actual shouldEqual
      """URLs and URLs in angle brackets will automatically get turned into links. http://www.example.com or <a href="http://www.example.com">http://www.example.com</a> and sometimes example.com (but not on Github, for example).""".stripMargin
  }
}
