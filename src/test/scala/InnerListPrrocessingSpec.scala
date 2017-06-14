import com.mdpeg.ASTTransform._
import com.mdpeg._
import org.scalatest.{FlatSpec, Matchers}

class InnerListPrrocessingSpec extends FlatSpec with Matchers {

  it should "process nested elements in an unordered list 2" in {
    val nulChar = "\0"

    val rawTree = Vector(UnorderedList(
      Vector(
        Markdown(
          """* item
            |    * sub 1
            |    * sub 2
            |    * sub 3
            |    * sub 4""".stripMargin))))


    rawTree |> transformTree shouldEqual
//      Right(Vector(
//        UnorderedList(Vector(
//          Plain(Vector(Text("item"))),
//          UnorderedList(Vector(
//            Plain(Vector(Text("sub"), Space, Text("1"))),
//            Plain(Vector(Text("sub"), Space, Text("2"))),
//            Plain(Vector(Text("sub"), Space, Text("3"))),
//            Plain(Vector(Text("sub"), Space, Text("4"))))
//          )))))
    Right(Vector(Vector(
      UnorderedList(Vector(
        Plain(Vector(Text("item"))),
        Plain(Vector(Text("sub"), Space, Text("1"))),
        UnorderedList(Vector(
          Plain(Vector(Text("sub"), Space, Text("2"))),
          UnorderedList(Vector(
            Plain(Vector(Text("sub"), Space, Text("3"))),
            UnorderedList(Vector(
              Plain(Vector(Text("sub"), Space, Text("4"))))))))))))))

//current result
    Right(Vector(Vector(
      UnorderedList(Vector(
        Plain(Vector(Text("item"))),
        Plain(Vector(Text("sub"), Space, Text("1"))),
        Plain(Vector(Text("sub"), Space, Text("2"))),
        Plain(Vector(Text("sub"), Space, Text("3"))),
        Plain(Vector(Text("sub"), Space, Text("4"))))
      ))))

  }
}