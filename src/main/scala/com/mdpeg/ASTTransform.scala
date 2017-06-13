package com.mdpeg

import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

object ASTTransform {
  type FailureMessage = String

  private def processMarkdown(m: Markdown): Either[FailureMessage, Vector[Block]] = {
    val Markdown(inline) = m
    val parser = new BlockParser(inline)
    parser.InputLine.run() match {
      case Success(node) => Right(node.toVector)
      case Failure(e: ParseError) =>
        val error = parser.formatError(e, new ErrorFormatter(showTraces = true))
        Left(error)
      case Failure(e) => sys.error(e.getMessage)
    }
  }

  private def transformNode(block: Block): Either[Vector[FailureMessage], Vector[Block]] = {
    def processMarkdownContainer[Container <: Block](v: Vector[Block])(resultMap: Vector[Block] => Either[Nothing, Vector[Container]]) = {
      def process(v: Vector[Block]) = transformTree(v) match {
        case Left(t) => Left(t)
        case Right(r) =>
          r.flatten |> transformTree match {
            case Left(errors) => Left(errors)
            case Right(blocks) => Right(blocks.flatten)
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

    def transformTable(mt: MultilineTableBlock) = {
      def parseColumn(body: MultilineTableColumn) = {
        body.
          map { case MultilineTableCell(blocks) => blocks }.
          map(processMarkdownContainer(_)(blocks => Right(Vector(MultilineTableCell(blocks))))) |>
          join |> (_.map(_.map { case Vector(MultilineTableCell(inner)) => MultilineTableCell(inner) }))
      }

      val MultilineTableBlock(relativeWidth, rawCaption, rawHead, rawBody) = mt
      val transformedCaption = rawCaption map { c =>
        val MultilineTableCaption(md, label) = c
        processMarkdownContainer(md)(blocks => Right(Vector(MultilineTableCaption(blocks, label))))
      }

      val transformedHead = rawHead map { head =>
        processMarkdownContainer {
          head.flatMap { case MultilineTableCell(bs) => bs }
        }(blocks => Right(Vector(MultilineTableCell(blocks))))
      }

      val transformedBodyColumns = rawBody.map(parseColumn) |> join

      (transformedCaption, transformedHead, transformedBodyColumns) match {
        case (Some(Left(l)), _, _) => Left(l)
        case (_, Some(Left(l)), _) => Left(l)
        case (_, _, Left(l)) => Left(l)

        case (c, h, b) =>
          val maybeCaption = c.map { x =>
            val Vector(MultilineTableCaption(blocks, label)) = x.right.get
            MultilineTableCaption(blocks, label)
          }
          val maybeHeader = h.map {
            _.right.get match { case Vector(MultilineTableCell(bs)) => bs.map(i => MultilineTableCell(Vector(i))) }
          }
          val body = b.right.get
          Right(Vector(MultilineTableBlock(relativeWidth, maybeCaption, maybeHeader, body)))
      }
    }

    block match {
      case m @ Markdown(_) => processMarkdownBlock(m)
      case UnorderedList(v) =>
        def unwrap(b:Vector[Block]) = b match {
          case Vector(UnorderedList(content)) => content
          case otherwise => otherwise
        }

        def toVector(e: Either[List[FailureMessage], List[Block]]) = e match {
          case Left(value) => Left(value.toVector)
          case Right(value) => Right(value.toVector)
        }
        def lift(b:Block) = Vector(b)
        def process(blocks: Seq[Block]): Either[List[FailureMessage], List[Block]] = {
          def sub(x: Block): Either[Vector[FailureMessage], Vector[Block]] = {
            val p = x |> lift
            processMarkdownContainer(p)(ps => Right(ps))
          }

          blocks match {
            case x :: xs => sub(x) match {
              case Left(l) => Left(l.toList)
              case Right(rr) => process(xs) match {
                case Left(ll) => Left(ll)
                case Right(rrr) =>
                  val a1 =rr |> unwrap |>(_.toList)
                  Right(a1 ::: rrr)
              }
            }
            case bs =>
              val a = List.empty[Block]
              Right(bs.toList)
          }
        }
        val parts: Vector[Block] = v.flatMap {
          case Markdown(ss) => ss.split("\0").map(_.replaceAll("\0", "")).filter(_!="").map(Markdown)
          case b => Vector(b)
        }

        parts.toList |> process |> toVector |> { p =>
          p.map(_.filter {
            case Plain(Vector(Space)) => false
            case otherwise => true
          })
        } |> {
          case l@Left(_) => l
          case Right(r) => Right(UnorderedList(r) |> lift)
        }

      case OrderedList(v) => processMarkdownContainer(v)(blocks => Right(Vector(OrderedList(blocks))))
      case BlockQuote(v) => processMarkdownContainer(v)(blocks => Right(Vector(BlockQuote(blocks))))
      case mt @ MultilineTableBlock(_, _, _, _) => transformTable(mt)
      case _ => Right(Vector(block))
    }
  }

  private def join[L, R](parsed: Seq[Either[Seq[L], Seq[R]]]): Either[Vector[L], Vector[Vector[R]]] = {
    parsed.partition(_.isLeft) match {
      case (Nil, blocks) => Right((for (Right(i) <- blocks) yield i.toVector).toVector)
      case (pe, _) =>
        val pes = for (Left(s) <- pe) yield s
        Left(pes.flatten.toVector)
    }
  }

  /**
    * Transforms a raw AST tree to a complete one (parses nested blocks, gathers links)
    * @param tree AST tree from Block parser
    * @return transformed AST tree or parse failure results, that is -- either tree having all nested markdown parsed or
    *         a list of parse failure messages
    */
  def transformTree(tree: Seq[Block]): Either[Vector[FailureMessage], Vector[Vector[Block]]] = {
    tree.map(transformNode) |> join
  }
}