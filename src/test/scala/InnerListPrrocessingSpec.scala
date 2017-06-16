import com.mdpeg.ASTTransform._
import com.mdpeg._
import org.parboiled2.{ErrorFormatter, ParseError}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class InnerListPrrocessingSpec extends FlatSpec with Matchers {

  def unwrapLC(c: Int)(b: List[Block]) : List[Block] = (c, b) match{
    case (count, List(UnorderedList(content))) if c > 0 => content.flatMap(List(_) |> unwrapLC(count - 1)).toList
    case (0, otherwise) => otherwise
    case (_, otherwise) => otherwise
  }


//  it should "process nested elements in an unordered list 2" in {
//    val nulChar = "\0"
//
//    val rawTree = Vector(UnorderedList(
//      Vector(
//        Markdown(
//          """* item
//            |    * sub 1
//            |    * sub 2
//            |    * sub 3
//            |    * sub 4""".stripMargin))))
//
//
//    val Vector(UnorderedList(Vector(Markdown(m)))) = rawTree
//    val aa = Markdown(m) |> processMarkdown
//
//    rawTree |> transformTree shouldEqual
//      Right(Vector(
//        UnorderedList(Vector(
//          Plain(Vector(Text("item"))),
//          UnorderedList(Vector(
//            Plain(Vector(Text("sub"), Space, Text("1"))),
//            Plain(Vector(Text("sub"), Space, Text("2"))),
//            Plain(Vector(Text("sub"), Space, Text("3"))),
//            Plain(Vector(Text("sub"), Space, Text("4"))))
//          )))))
////    Right(Vector(Vector(
////      UnorderedList(Vector(
////        Plain(Vector(Text("item"))),
////        Plain(Vector(Text("sub"), Space, Text("1"))),
////        UnorderedList(Vector(
////          Plain(Vector(Text("sub"), Space, Text("2"))),
////          UnorderedList(Vector(
////            Plain(Vector(Text("sub"), Space, Text("3"))),
////            UnorderedList(Vector(
////              Plain(Vector(Text("sub"), Space, Text("4"))))))))))))))
//
////current result
//    Right(Vector(Vector(
//      UnorderedList(Vector(
//        Plain(Vector(Text("item"))),
//        Plain(Vector(Text("sub"), Space, Text("1"))),
//        Plain(Vector(Text("sub"), Space, Text("2"))),
//        Plain(Vector(Text("sub"), Space, Text("3"))),
//        Plain(Vector(Text("sub"), Space, Text("4"))))
//      ))))
//
//  }
  def parse(inline: String): Any = {
    val parser: BlockParser = new BlockParser(inline)
    parser.InputLine.run() match {
      case s@Success(node) => s
      case Failure(e: ParseError) =>
        Left(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => sys.error(e.getMessage)
    }
  }

  it should "process nested elements in an unordered list 2" in {
    val nulChar = "\0"
    val tab ="\t"
    val rawTree = Vector(UnorderedList(
      Vector(Markdown(s"""item 1
                      |${nulChar}    - sub 1
                      |    - sub 2""".stripMargin),
          Markdown(s"""item 2
                      |${nulChar}  - sub 3
                      |  - sub 4""".stripMargin))))

    val aa = s"""- item
                |    - sub 1
                |    - sub 2""".stripMargin |> parse
    println(aa)
    rawTree |> transformTree shouldEqual
      Right(Vector(
        UnorderedList(Vector(
          Plain(Vector(Text("item"), Space, Text("1"))),
          UnorderedList(Vector(
            Plain(Vector(Text("sub"), Space, Text("1"))),
            Plain(Vector(Text("sub"), Space, Text("2"))))),
          Plain(Vector(Text("item"), Space, Text("2"))),
          UnorderedList(Vector(
            Plain(Vector(Text("sub"), Space, Text("3"))),
            Plain(Vector(Text("sub"), Space, Text("4")))))
          ))))
  }

  it should "unwrap tree" in {
    val tree = Vector(Vector(
          UnorderedList(Vector(
            Plain(Vector(Text("item"))),
            Plain(Vector(Text("sub"), Space, Text("1"))),
            UnorderedList(Vector(
              Plain(Vector(Text("sub"), Space, Text("2"))),
              UnorderedList(Vector(
                Plain(Vector(Text("sub"), Space, Text("3"))),
                UnorderedList(Vector(
                  Plain(Vector(Text("sub"), Space, Text("4")))))))))))))

    tree.map(t => unwrapLC(0)(t.toList)) shouldEqual
    Vector(List(UnorderedList(Vector(Plain(Vector(Text("item"))),
      Plain(Vector(Text("sub"), Space, Text("1"))),
      UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("2"))),
        UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("3"))),
          UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("4")))))))))))))

    tree.map(t => unwrapLC(1)(t.toList)) shouldEqual
    Vector(List(
      Plain(Vector(Text("item"))),
      Plain(Vector(Text("sub"), Space, Text("1"))),
      UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("2"))),
        UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("3"))),
          UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("4"))))))
        )))))

    tree.map(t => unwrapLC(2)(t.toList)) shouldEqual
      Vector(List(
        Plain(Vector(Text("item"))),
        Plain(Vector(Text("sub"), Space, Text("1"))),
        Plain(Vector(Text("sub"), Space, Text("2"))),
        UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("3"))),
          UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("4")))))))))

    tree.map(t => unwrapLC(3)(t.toList)) shouldEqual
      Vector(List(
        Plain(Vector(Text("item"))),
        Plain(Vector(Text("sub"), Space, Text("1"))),
        Plain(Vector(Text("sub"), Space, Text("2"))),
        Plain(Vector(Text("sub"), Space, Text("3"))),
        UnorderedList(Vector(Plain(Vector(Text("sub"), Space, Text("4")))))))

    tree.map(t => unwrapLC(4)(t.toList)) shouldEqual
      Vector(List(
        Plain(Vector(Text("item"))),
        Plain(Vector(Text("sub"), Space, Text("1"))),
        Plain(Vector(Text("sub"), Space, Text("2"))),
        Plain(Vector(Text("sub"), Space, Text("3"))),
        Plain(Vector(Text("sub"), Space, Text("4")))))
  }
}