import com.mdpeg._
import org.parboiled2.{ErrorFormatter, ParseError, Parser, ParserInput}

import scala.util.{Failure, Success}

object PrettyPrint1 {
  def apply(parser : ListBlockParser) : Unit= {
    val result= parser.bulletListItem.run()
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

class P(val input: ParserInput) extends Parser with ListBlockParser{
}

val term = s"""- item 1""".stripMargin

PrettyPrint1(new P(term))