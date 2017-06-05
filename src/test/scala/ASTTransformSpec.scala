import com.mdpeg.ASTTransform._
import com.mdpeg._
import org.scalatest.{FlatSpec, Matchers}

class ASTTransformSpec extends FlatSpec with Matchers {

  it should "Should extract one level of Markdown to block seq" in {
    val term =
      s"""> ${TestData.blockQuoteLineOne}
         |> ${TestData.blockQuoteLineTwo}
         |> ${TestData.blockQuoteLineThree}
         |""".stripMargin
    val parsed = new BlockParser(term).blockQuote.run().get

//    parsed.inline.map(transformNode).map(_.getOrElse(Vector.empty[Block])) shouldEqual
//    Vector(
//      Vector(
//        Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote")))),
//      Vector(
//        Plain(Vector(Text("and"), Space, Text("should"), Space, Text("span"), Space, Text("several")))),
//      Vector(
//        Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space, Text("the"), Space, Text("block"))))
//    )

    parsed.inline |> transformTree  shouldEqual
    Right(
      Vector(
        Vector(Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote")))),
        Vector(Plain(Vector(Text("and"), Space, Text("should"), Space, Text("span"), Space, Text("several")))),
        Vector(Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space, Text("the"), Space, Text("block")))))
    )
  }
}
