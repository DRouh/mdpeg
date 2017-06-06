import com.mdpeg.ASTTransform._
import com.mdpeg._
import org.scalatest.{FlatSpec, Matchers}

class ASTTransformSpec extends FlatSpec with Matchers {

  it should "parse Markdown blocks to block seq" in {
    val rawAstTree = Vector(
      Markdown("This is quote"),
      Markdown("and should span several"),
      Markdown("yet another line for the block")
    )

    val transformedTree = rawAstTree |> transformTree

    transformedTree shouldEqual
    Right(
      Vector(
        Vector(Plain(Vector(Text("This"), Space, Text("is"), Space, Text("quote")))),
        Vector(Plain(Vector(Text("and"), Space, Text("should"), Space, Text("span"), Space, Text("several")))),
        Vector(Plain(Vector(Text("yet"), Space, Text("another"), Space, Text("line"), Space, Text("for"), Space, Text("the"), Space, Text("block")))))
    )
  }
}
