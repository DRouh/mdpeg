package com.mdpeg

import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

object ASTTransform {
  type FailureMessage = String

  def lift(b: Block) = Vector(b)

  def toVector(e: Either[List[FailureMessage], List[Block]]) = e match {
    case Left(value) => Left(value.toVector)
    case Right(value) => Right(value.toVector)
  }

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

  def processMarkdown(m: Markdown): Either[Vector[FailureMessage], Vector[Block]] = {
    def parse(inline: String): Either[String, Vector[Block]] = {
      val parser: BlockParser = new BlockParser(inline)
      parser.InputLine.run() match {
        case Success(node) => Right(node.toVector)
        case Failure(e: ParseError) => Left(parser.formatError(e, new ErrorFormatter(showTraces = true)))
        case Failure(e) => sys.error(e.getMessage)
      }
    }
    val Markdown(inline) = m
    val (pre, cont) = inline splitToTuple "\0"

    pre |> parse match {
      case Left(value) => Left(Vector(value))
      case Right(value) if cont == "" => Right(value)
      case Right(firstValue) => Vector(Markdown(cont)) |> transformTree match {
        case Left(value) => Left(value)
        case Right(secondValue) => Right((firstValue.toList ::: secondValue.flatten.toList).toVector)
      }
    }
  }

  private def transformNode(block: Block): Either[Vector[FailureMessage], Vector[Block]] = {
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

    def processUnorderedList(v: Vector[Block]): Either[Vector[FailureMessage], Vector[Block]] = {
      def unwrap(b: Vector[Block]): Vector[Block] = b match {
        case Vector(UnorderedList(content)) => content flatMap (_ |> lift |> unwrap)
        case otherwise => otherwise
      }

      def toVector(e: Either[List[FailureMessage], List[Block]]) = e match {
        case Left(value) => Left(value.toVector)
        case Right(value) => Right(value.toVector)
      }

      def lift(b: Block) = Vector(b)

      def process(blocks: Seq[Block]): Either[List[FailureMessage], List[Block]] = {
        def listProcess(x: Block, xs: List[Block]) = {
          sub(x) match {
            case Left(l) => Left(l.toList)
            case Right(rr) => process(xs) match {
              case Left(ll) => Left(ll)
              case Right(rrr) => Right(rr.toList ::: rrr)
            }
          }
        }

        def sub(x: Block): Either[Vector[FailureMessage], Vector[Block]] = {
          val p = x |> lift
          processMarkdownContainer(p)(ps => Right(ps))
        }

        blocks match {
          case x :: Nil => sub(x) match {
            case l @ Left(value) => Left(value.toList)
            //case Right(value) => Right(value |> unwrap |> (_.toList))
            case Right(value) => Right(value.toList)
          }

          case x :: xs => listProcess(x, xs)
          case otherwise => Right(otherwise.toList)
        }
      }

      val parts: Vector[Block] = v.flatMap {
        case Markdown(ss) => ss.split("\0").map(_.replaceAll("\0", "")).filter(_ != "").map(Markdown)
        case otherwise => Vector(otherwise)
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
    }

    def processUnorderedList2(v: Vector[Block]): Either[Vector[FailureMessage], Vector[Block]] = {

      def unwrapL(b: List[Block]): List[Block] = b match {
        case List(UnorderedList(content)) => (content flatMap (List(_) |> unwrapL)) |> (_.toList)
        case otherwise => otherwise
      }

      def unwrapLC(c: Int)(b: List[Block]) : List[Block] = (c, b) match{
        case (c, List(UnorderedList(content))) if c > 0 => (content flatMap (List(_) |> unwrapLC(c - 1))) |> (_.toList)
        case (0, otherwise) => otherwise
        case (_, otherwise) => otherwise
      }

      def toVector(e: Either[List[FailureMessage], List[Block]]) = e match {
        case Left(value) => Left(value.toVector)
        case Right(value) => Right(value.toVector)
      }

      def process(c: Int)(blocks: Seq[Block]): Either[List[FailureMessage], List[Block]] = {
        def listProcess(x: Block, xs: List[Block]) = {
          sub(x) match {
            case Left(l) => Left(l.toList)
            case Right(rr) => process(c + 1)(xs) match {
              case Left(ll) => Left(ll)
              case Right(rrr) => Right(rr.toList ::: rrr)
            }
          }
        }

        def sub(x: Block): Either[Vector[FailureMessage], Vector[Block]] = x match {
          case m@Markdown(_) => m |> processMarkdown match {
            case Left(value) => Left(value)
            case Right(value) => (value map (_ |> transformNode)) |> join |> (_ map (_.flatten))
          }
          case otherwise => Right(Vector(otherwise))
        }

        blocks match {
          case x :: Nil => sub(x) match {
            case l @ Left(value) => Left(value.toList)
            case Right(value) => Right(value.toList |> unwrapLC(c))
            //case Right(value) => Right(value.toList)
          }

          case x :: xs => listProcess(x, xs)
          case otherwise => Right(otherwise.toList |> unwrapL)
        }
      }
      v.toList |> process(2) |> toVector |> { p =>
        p.map(_.filter {
          case Plain(Vector(Space)) => false
          case otherwise => true
        })
      } |> {
        case l@Left(_) => l
        case Right(r) => Right(UnorderedList(r) |> lift)
      }
    }

    block match {
      case m @ Markdown(_) => processMarkdown(m)
      case UnorderedList(v) => processUnorderedList2(v)

      case OrderedList(v) => processMarkdownContainer(v)(blocks => Right(Vector(OrderedList(blocks))))
      case BlockQuote(v) => processMarkdownContainer(v)(blocks => Right(Vector(BlockQuote(blocks))))
      case mt @ MultilineTableBlock(_, _, _, _) => transformTable(mt)
      case _ => Right(Vector(block))
    }
  }

  private def transformNode2(uoCount: Int = 1)(block: Block): Either[Vector[FailureMessage], Vector[Block]] = {
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
    def processUnorderedList2(count: Int)(v: Vector[Block]): Either[Vector[FailureMessage], Vector[Block]] = {
      def unwrapLC(c: Int)(b: List[Block]) : List[Block] = (c, b) match{
        case (count, List(UnorderedList(content))) if c > 0 => content.flatMap(List(_) |> unwrapLC(count - 1)).toList
        case (0, otherwise) => otherwise
        case (_, otherwise) => otherwise
      }

      def process(c: Int)(blocks: Seq[Block]): Either[List[FailureMessage], List[Block]] = {
        def listProcess(x: Block, xs: List[Block]) = {
          sub(x) match {
            case Left(l) => Left(l.toList)
            case Right(rr) => process(c + 1)(xs) match {
              case Left(ll) => Left(ll)
              case Right(rrr) => Right(rr.toList ::: rrr)
            }
          }
        }

        def sub(x: Block): Either[Vector[FailureMessage], Vector[Block]] = x match {
          case m@Markdown(_) => m |> processMarkdown match {
            case Left(value) => Left(value)
            case Right(value) => (value map (_ |> transformNode2(count))) |> join |> (_ map (_.flatten))
          }
          case otherwise => Right(Vector(otherwise))
        }

        blocks match {
          case x :: Nil => sub(x) match {
            case l @ Left(value) => Left(value.toList)
            case Right(value) => Right(value.toList |> unwrapLC(count))
            //case Right(value) => Right(value.toList)
          }

          case x :: xs => listProcess(x, xs)
          case otherwise => Right(otherwise.toList)
        }
      }
      v.toList |> process(count) |> toVector |> { p =>
        p.map(_.filter {
          case Plain(Vector(Space)) => false
          case otherwise => true
        })
      } |> {
        case l@Left(_) => l
        case Right(r) => Right(UnorderedList(r) |> lift)
      }
    }

    block match {
      case m @ Markdown(_) => processMarkdown(m)
      case UnorderedList(v) => processUnorderedList2(uoCount + 1)(v)

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