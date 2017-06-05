package com.mdpeg

import org.parboiled2.{ErrorFormatter, ParseError}
import scala.util.{Failure, Success}

object ASTTransform {
  type FailureMessage = String
  type MacroBlock = Seq[Block]

  def processMarkdown(m: Markdown): Either[FailureMessage, Seq[Block]] = {
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

  def transformNode(b: Block): Either[FailureMessage, Seq[Block]] = {
    b match {
      case m @ Markdown(_) =>
        processMarkdown(m) match {
          case l @ Left(_) => l
          case r @ Right(_) => r
        }
      case _  => Right(Vector(b))
    }
  }

  def join(parsed: Seq[Either[FailureMessage, Seq[Block]]]): Either[Seq[FailureMessage], Seq[MacroBlock]] = {
    parsed.partition(_.isLeft) match {
      case(Nil, blocks) =>
        val mbs: Seq[MacroBlock] = for(Right(i) <- blocks) yield i
        Right(mbs)
      case(pe, _) =>
        val pes: Seq[FailureMessage] = for(Left(s) <- pe) yield s
        Left(pes)
    }
  }

  def transformTree(tree: Seq[Block]): Either[Seq[FailureMessage], Seq[MacroBlock]] = {
    tree.map(transformNode) |> join
  }
}
