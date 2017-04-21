import com.mdpeg.BlockParser
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}
object PrettyPrint1 {
  def apply(parser : BlockParser) : Unit= {
    val result= parser.InputLine.run()
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
val input1 ="""It is a long established fact that a reader will be distracted by the
              |readable content of a page when looking at its layout. The point of
              |using Lorem Ipsum is that it has a more-or-less normal distribution of
              |letters, as opposed to using 'Content here, content here', making it look
              |like readable English.
              |
              |Many desktop publishing packages and web page editors now use Lorem Ipsum as
              |their default model text, and a search for 'lorem ipsum' will uncover many web
              |sites still in their infancy. Various versions have evolved over the years,
              |sometimes by accident, sometimes on purpose (injected humour and the like).
              |The following system is designed for Berg Analytics analysts.""".stripMargin

PrettyPrint1(new BlockParser(input1))