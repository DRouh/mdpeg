package com.mdpeg

import org.parboiled2.{ErrorFormatter, ParseError}
import scala.util.{Failure, Success}

object ASTTransform {

  def processMarkdown(m: Markdown): Either[String, Seq[Block]] = {
    val Markdown(inline) = m
    val parser = new BlockParser(inline)
    parser.InputLine.run() match {
      case Success(node) => Right(node)
      case Failure(e: ParseError) =>
        val error = parser.formatError(e, new ErrorFormatter(showTraces = true))
        Left(error)
      case Failure(e) => sys.error(e.getMessage)
    }
  }

  def transformNode(b: Block): Either[String, Seq[Block]] = {
    b match {
      case m @ Markdown(_) =>
        processMarkdown(m) match {
          case l @ Left(_) => l
          case r @ Right(_) => r
        }
      case _  => Right(Vector(b))
    }
  }

  def transformTree(tree: Seq[Block]): Either[ParseError, Seq[Block]] = {
    ???
  }
}
