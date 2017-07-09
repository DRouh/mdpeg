package org.mdpeg

import org.mdpeg.ASTTransform.{AstTransformError, extractLinks, transformTree}
import org.mdpeg.OutputTransform.{HtmlContent, toHtml}
import org.mdpeg.ast.Ast
import org.mdpeg.parsers.BlockParser
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

object MarkdownParser {
  type HtmlTransformFail = String

  /**
    * Parses given input string
    * @param input string content to be parsed
    * @return Either an array of fail messages or an AST (Abstract Syntax Tree)
    */
  def parse(input: String): Either[Vector[AstTransformError], Ast] = {
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
  def transformToHtml(ast: Ast): Either[HtmlTransformFail, HtmlContent] =
    try {
      Right(ast |> { t => (t |> extractLinks |> toHtml) (t) })
    }
    catch {
      case e: Throwable => Left(e.getMessage)
    }
}
