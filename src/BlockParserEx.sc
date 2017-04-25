import com.mdpeg._
import org.parboiled2.{ErrorFormatter, ParseError}
import scala.util.{Failure, Success}

object PrettyPrint1 {
  def apply(parser : BlockParser) : Unit= {
    val result= parser.verbatim.run()
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

val term =
  s"""```javascript
         |var s = "JavaScript syntax highlighting";
         |alert(s);
         |```
         |
      """.stripMargin

PrettyPrint1(new BlockParser(term))