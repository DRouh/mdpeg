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
        val transformedCaption: Option[Either[Seq[FailureMessage], Seq[Block]]] = caption map { c =>
          val MultilineTableCaption(md) = c
          processMarkdownContainer(md)(blocks => Right(Vector(MultilineTableCaption(blocks.toVector))))
        }

        val transformedHead: Option[Either[Seq[FailureMessage], Seq[Block]]] = head map { head =>
          val cells = head.flatMap { h =>
            val MultilineTableCell(z) = h
            z
          } // ToDo Validate that flatten is correct here
          processMarkdownContainer(cells)(blocks => Right(Vector(MultilineTableCell(blocks.toVector))))
        }

        val transformedBody: Vector[Either[Seq[FailureMessage], Vector[MultilineTableCell]]] = body map { body =>
          body.map { h =>
              val MultilineTableCell(blocks) = h
              blocks
            }
            .map(processMarkdownContainer(_)(blocks => Right(Vector(MultilineTableCell(blocks.toVector))))) |>
            join |>
            (_.map(x => x.map(c => MultilineTableCell(c.toVector)).toVector))
        }

        // ToDo this is unsafe and should be re-implemented
        val transformedBodyColumns: Either[Seq[FailureMessage], Vector[MultilineTableColumn]] =
          transformedBody |> join |> (_.map(_.map(_.toVector).toVector))

        (transformedCaption, transformedHead, transformedBodyColumns) match {
          case (Some(Left(l)), _, _) =>  Left(l)
          case (_, Some(Left(l)), _) => Left(l)
          case (_, _,  Left(l)) =>  Left(l)

          case (c,h,b) =>
            val cc = c.map(x => MultilineTableCaption(x.right.get.toVector))
            val hh= h.map(y => Vector(MultilineTableCell(y.right.get.toVector)))
            val bb = b.right.get
            Right(Vector(MultilineTableBlock(relativeWidth, cc, hh, bb)))
        }
      case _ => Right(Vector(block))
    }
  }

  def join[L, R](parsed: Seq[Either[Seq[L], Seq[R]]]): Either[Seq[L], Seq[Seq[R]]] = {
    parsed.partition(_.isLeft) match {
      case (Nil, blocks) => Right(for (Right(i) <- blocks) yield i)
      case (pe, _) =>
        val pes = for (Left(s) <- pe) yield s
        Left(pes.flatten)
    }
  }

  def transformTree(tree: Seq[Block]): Either[Seq[FailureMessage], Seq[MacroBlock]] = {
    tree.map(transformNode) |> join
  }
}
