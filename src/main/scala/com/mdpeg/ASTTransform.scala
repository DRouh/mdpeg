package com.mdpeg

import org.parboiled2.{ErrorFormatter, ParseError}

import scala.collection.immutable
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

  def transformNode(block: Block): Either[Seq[FailureMessage], Seq[Block]] = {
    def processMarkdownContainer[Container <: Block](v: Seq[Block])(resultMap: Seq[Block] => Either[Nothing, Seq[Container]]) = {
      def process(v: Seq[Block]) = {
        transformTree(v) match {
          case Left(t) => Left(t)
          case Right(r) =>
            r.flatten |> transformTree match {
              case Left(errors) => Left(errors)
              case Right(blocks) => Right(blocks.flatten)
            }
        }
      }

      process(v) match {
        case l @ Left(_) => l
        case Right(blocks) => resultMap(blocks)
      }
    }

    def processMarkdownBlock(m: Markdown) = {
      processMarkdown(m) match {
        case Left(errors) => Left(Vector(errors))
        case Right(blocks) => Right(blocks)
      }
    }

    block match {
      case m @ Markdown(_) => processMarkdownBlock(m)
      case UnorderedList(v) => processMarkdownContainer(v)(blocks => Right(Vector(UnorderedList(blocks.toVector))))
      case OrderedList(v) => processMarkdownContainer(v)(blocks => Right(Vector(OrderedList(blocks.toVector))))
      case BlockQuote(v) => processMarkdownContainer(v)(blocks => Right(Vector(BlockQuote(blocks.toVector))))
      case MultilineTableBlock(relativeWidth, caption, head, body) =>
        val transformedCaption = caption map { c =>
          val MultilineTableCaption(md) = c
          processMarkdownContainer(md)(blocks => Right(Vector(MultilineTableCaption(blocks.toVector))))
        }

        val transformedHead = head map { head =>
          val cells = head.flatMap { h =>
            val MultilineTableCell(blocks) = h
            blocks
          } // ToDo Validate that flatten is correct here
          processMarkdownContainer(cells)(blocks => Right(Vector(MultilineTableCell(blocks.toVector))))
        }

        val transformedBody: Vector[Seq[Either[Seq[FailureMessage], Seq[Block]]]] = body map { body =>
          val rows: immutable.Seq[Vector[Block]] = body.map { h =>
            val MultilineTableCell(blocks) = h
            blocks
          } // ToDo Validate that flatten is correct here

          rows.map { cells =>
            processMarkdownContainer(cells)(blocks => Right(Vector(MultilineTableCell(blocks.toVector))))
          }
        }

        // ToDo monadic check for eithers

        // ToDo this is unsafe and should be re-implemented
        val transformedBodyColumns: Vector[MultilineTableColumn] = transformedBody.map { b =>
          b.map(c => MultilineTableCell(c.right.get.toVector)).toVector
        }

        val result = MultilineTableBlock(relativeWidth, caption, head, transformedBodyColumns)
        Right(Vector(result))
      case _ => Right(Vector(block))
    }
  }

  def join(parsed: Seq[Either[Seq[FailureMessage], Seq[Block]]]): Either[Seq[FailureMessage], Seq[MacroBlock]] = {
    parsed.partition(_.isLeft) match {
      case (Nil, blocks) =>
        val mbs: Seq[MacroBlock] = for (Right(i) <- blocks) yield i
        Right(mbs)
      case (pe, _) =>
        val pes: Seq[Seq[FailureMessage]] = for (Left(s) <- pe) yield s
        Left(pes.flatten)
    }
  }

  def transformTree(tree: Seq[Block]): Either[Seq[FailureMessage], Seq[MacroBlock]] = {
    tree.map(transformNode) |> join
  }
}
