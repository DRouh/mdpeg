package com.mdpeg

import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

object ASTTransform {
  type FailureMessage = String

  /**
    * Transforms a raw AST tree to a complete one (parses nested blocks, gathers links)
    *
    * @param tree AST tree from Block parser
    * @return transformed AST tree or parse failure results, that is -- either tree having all nested markdown parsed or
    *         a list of parse failure messages
    */
  def transformTree(tree: Seq[Block]): Either[Vector[FailureMessage], Vector[Vector[Block]]] = {
    tree.map(transformNode) |> halfJoin
  }

  def extractLinks(tree: Vector[Vector[Block]]): Vector[(InlineContent, String, Option[String])] =
    tree.flatten.filter {
      case ReferenceBlock(_, Src(_, _)) => true
      case otherwise => false
    }.map { case ReferenceBlock(l, Src(s, t)) => (l, s, t) }

  private def liftV(b: Block) = Vector(b)

  private def eitherToVector(e: Either[List[FailureMessage], List[Block]]) = e match {
    case Left(value) => Left(value.toVector)
    case Right(value) => Right(value.toVector)
  }

  private def processMarkdownContainer(v: Vector[Block])(resultMap: Vector[Block] => Vector[Block]):
  Either[Vector[FailureMessage], Vector[Block]] = {
    def process(v: Vector[Block]) = transformTree(v) match {
      case Left(t) => Left(t)
      case Right(r) => r.flatten |> transformTree match {
        case Left(errors) => Left(errors)
        case Right(blocks) => Right(blocks.flatten)
      }
    }

    process(v) match {
      case l@Left(_) => l
      case Right(blocks) => Right(resultMap(blocks))
    }
  }

  private def processMarkdown(m: Markdown): Either[Vector[FailureMessage], Vector[Block]] = {
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

  private def processList(v: Vector[Block])(resultMap: Vector[Block] => Vector[Block]):
  Either[Vector[FailureMessage], Vector[Block]] = {
    def unwrap(b: Vector[Block]): Vector[Block] = b match {
      case Vector(UnorderedList(content)) => content flatMap (_ |> liftV |> unwrap)
      case Vector(OrderedList(content)) => content flatMap (_ |> liftV |> unwrap)
      case otherwise => otherwise
    }

    def process(blocks: Seq[Block]): Either[List[FailureMessage], List[Block]] = {
      def listProcess(x: Block, xs: List[Block]) = {
        sub(x) match {
          case Left(l) => Left(l.toList)
          case Right(rr) => process(xs) match {
            case Left(ll) => Left(ll)
            case Right(rrr) => Right((rr |> unwrap |> (_.toList)) ::: (rrr.toVector |> unwrap |> (_.toList)))
          }
        }
      }

      def sub(x: Block): Either[Vector[FailureMessage], Vector[Block]] = processMarkdownContainer(x |> liftV)(identity)

      blocks match {
        case x :: Nil => sub(x) match {
          case Left(value) => Left(value.toList)
          case Right(value) => Right(value |> unwrap |> (_.toList))
        }
        case x :: xs => listProcess(x, xs)
        case otherwise => Right(otherwise.toList)
      }
    }

    val parts: Vector[Block] = v.flatMap { case Markdown(ss) => ss.split("\0").map(_.replaceAll("\0", "")).filter(_
      != "").map(Markdown)
    case otherwise => Vector(otherwise)
    }

    parts.toList |> process |> eitherToVector |> { p =>
      p.map(_.filter { case Plain(Vector(Space)) => false
      case otherwise => true
      })
    } |> { case l@Left(_) => l
    case Right(r) => Right(resultMap(r))
    }
  }

  private def transformTable(mt: MultilineTableBlock) = {
    def parseColumn(body: MultilineTableColumn) = {
      body.map { case MultilineTableCell(blocks) => blocks }.
        map(processMarkdownContainer(_)(blocks => Vector (MultilineTableCell(blocks)))) |>
        halfJoin |>
        (_.map(_.map { case Vector(MultilineTableCell(inner)) => MultilineTableCell(inner) }))
    }

    val MultilineTableBlock(relativeWidth, rawCaption, rawHead, rawBody) = mt
    val transformedCaption = rawCaption map { c =>
      val MultilineTableCaption(md, label) = c
      processMarkdownContainer(md)(blocks => Vector(MultilineTableCaption(blocks, label)))
    }

    val transformedHead = rawHead map { head =>
      processMarkdownContainer {
        head.flatMap { case MultilineTableCell(bs) => bs }
      }(blocks => Vector(MultilineTableCell(blocks)))
    }

    val transformedBodyColumns = rawBody.map(parseColumn) |> halfJoin

    (transformedCaption, transformedHead, transformedBodyColumns) match {
      case (Some(Left(l)), _, _) => Left(l)
      case (_, Some(Left(l)), _) => Left(l)
      case (_, _, Left(l)) => Left(l)
      case (c, h, b) => val maybeCaption = c.map { x =>
        val Vector(MultilineTableCaption(blocks, label)) = x.right.get
        MultilineTableCaption(blocks, label)
      }
        val maybeHeader = h.map {
          _.right.get match {
            case Vector(MultilineTableCell(bs)) => bs.map(i => MultilineTableCell(Vector(i)))
          }
        }
        val body = b.right.get
        Right(Vector(MultilineTableBlock(relativeWidth, maybeCaption, maybeHeader, body)))
    }
  }

  private def transformNode(block: Block): Either[Vector[FailureMessage], Vector[Block]] = {
    block match {
      case Markdown("") => Right(Vector(Plain(Vector(Text("")))))
      case m@Markdown(_) => processMarkdown(m)
      case UnorderedList(v) => processList(v)(r => UnorderedList(r) |> liftV)
      case OrderedList(v) => processList(v)(r => OrderedList(r) |> liftV)
      case BlockQuote(v) => processMarkdownContainer(v)(blocks => Vector(BlockQuote(blocks)))
      case mt@MultilineTableBlock(_, _, _, _) => transformTable(mt)
      case _ => Right(Vector(block))
    }
  }

  private def halfJoin[L, R](parsed: Seq[Either[Seq[L], Seq[R]]]): Either[Vector[L], Vector[Vector[R]]] = {
    parsed.partition(_.isLeft) match {
      case (Nil, blocks) => Right((for (Right(i) <- blocks) yield i.toVector).toVector)
      case (pe, _) => val pes = for (Left(s) <- pe) yield s
        Left(pes.flatten.toVector)
    }
  }
}