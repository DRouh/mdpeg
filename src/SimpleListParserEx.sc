import com.mdpeg.{ListItem, SimpleListParser}
import org.parboiled2._

import scala.collection.immutable
import scala.util.{Failure, Success, Try}

object PrettyPrint {
  def apply(parser : SimpleListParser) : Unit= {
    val result: Try[immutable.Seq[ListItem]] = parser.InputLine.run()
    result match {
      case Failure(error) =>
        error match {
          case e : ParseError => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
          case _ => println(error)
        }
      case Success(value) => println(value)
    }

  }
}

val input1 = """1. It is a long - established fact that a reader will be;"""
val input2 =
  """1. 1. It is a long - established fact that a reader will be;
    |2. The point of using - Lorem Ipsum is that it has a more-or-less normal ;
    |3. Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for 'lorem ipsum'.""".stripMargin

PrettyPrint(new SimpleListParser(input1))
PrettyPrint(new SimpleListParser(input2))