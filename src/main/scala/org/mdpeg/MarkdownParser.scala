package org.mdpeg

import org.mdpeg.ASTTransform.{FailureMessage, transformTree, extractLinks}
import org.mdpeg.OutputTransform.{HtmlContent,toHtml}
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

/**
  * Created by Dime Rouh on 01.07.2017.
  */
object MarkdownParser {
  type FailMessage = String
  type AST = Vector[Vector[Block]]

  /**
    * Parses given input string
    * @param input string content to be parsed
    * @return Either an array of fail messages or an AST (Abstract Syntax Tree)
    */
  def parse(input: String): Either[Vector[FailureMessage], AST] = {
    val parser: BlockParser = new BlockParser(input)
    parser.InputLine.run() match {
      case Success(rawAstTree) =>
        rawAstTree |> transformTree match {
          case Left(errors) => Left(errors)
          case Right(transformedTree) => Right(transformedTree)
        }
      case Failure(e: ParseError) => Left(Vector(parser.formatError(e, new ErrorFormatter(showTraces = true))))
      case Failure(e) => Left(Vector(e.getMessage))
    }
  }

  /**
    * Transforms given AST to an HTMl output.
    * @param ast an AST (Abstract Syntax Tree)
    * @return an HTML content string
    */
  def transformToHtml(ast: AST): Either[FailMessage, HtmlContent] =
    try {
      Right(ast |> { t => (t |> extractLinks |> toHtml) (t) })
    }
    catch {
      case e: Throwable => Left(e.getMessage)
    }
}
