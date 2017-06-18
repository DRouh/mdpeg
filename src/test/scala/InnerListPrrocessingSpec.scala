import com.mdpeg.ASTTransform._
import com.mdpeg._
import org.parboiled2.{ErrorFormatter, ParseError}
import org.scalatest.{FlatSpec, Ignore, Matchers}

import scala.util.{Failure, Success}

@Ignore
class InnerListPrrocessingSpec extends FlatSpec with Matchers {

  def unwrapLC(c: Int)(b: List[Block]) : List[Block] = (c, b) match{
    case (count, List(UnorderedList(content))) if c > 0 => content.flatMap(List(_) |> unwrapLC(count - 1)).toList
    case (0, otherwise) => otherwise
    case (_, otherwise) => otherwise
  }

  def parse(inline: String): Either[Any, Vector[Block]] = {
    val parser: BlockParser = new BlockParser(inline)
    parser.InputLine.run() match {
      case s@Success(node) => Right(node.toVector)
      case Failure(e: ParseError) =>
        Left(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => sys.error(e.getMessage)
    }
  }

  it should "process nested elements in an unordered list 2" in {
    val nulChar = "\0"
    val tab = "\t"
    val rawTree = Vector(UnorderedList(
      Vector(Markdown(
        s"""item 1
           |${nulChar}    - sub 1
           |    - sub 2""".stripMargin),
        Markdown(
          s"""item 2
             |${nulChar}  - sub 3
             |  - sub 4""".stripMargin))))

    s"""- item 1
       |    - sub 1
       |    - sub 2
       |- item 2
       |  - sub 3
       |  - sub 4""".stripMargin |>
      parse |> (_.map { case Vector(UnorderedList(Vector(mm@Markdown(_), mmm@Markdown(_)))) => for (x <- mm |> processMarkdown; y <- mmm |> processMarkdown) yield (x, y)
    }) shouldEqual
      Right(Right((
        Vector(
          Plain(Vector(Text("item"), Space, Text("1"))),
          UnorderedList(Vector(Markdown(
            s"""sub 1
               |${nulChar}    - sub 2""".stripMargin)))),
        Vector(Plain(Vector(Text("item"), Space, Text("2"))),
          UnorderedList(Vector(Markdown(
            s"""sub 3
               |${nulChar}  - sub 4""".stripMargin)))))))
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