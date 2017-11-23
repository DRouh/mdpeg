package org.mdpeg

import org.mdpeg.ast._
import org.mdpeg.parsers.BlockParser
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.language.implicitConversions
import scala.util.{Failure, Success}

private[mdpeg] object ASTTransform {
  type AstTransformError = String

  /**
    * Transforms a raw AST tree to a complete one (parses nested blocks, gathers links)
    *
    * @param tree AST tree from Block parser
    * @return transformed AST tree or parse failure results, that is -- either tree having all nested markdown parsed or
    *         a list of parse failure messages
    */
  def transformTree(tree: Seq[Block]): Either[Vector[AstTransformError], Ast] = {
    tree.map(transformNode) |> halfJoin |> (_.map(vs => Ast(vs)))
  }

  def extractLinks(tree: Ast): Map[InlineContent, (String, Option[String])] = {
    tree
      .flatten.collect { case ReferenceBlock(k, s: Src) => k -> s }
      .groupBy(_._1)
      .flatMap { case (key, values) => for (h <- values.headOption.map(_._2)) yield (key, (h.uri, h.title)) }
  }

  private def liftV(b: Block) = Vector(b)

  private def eitherToVector(e: Either[List[AstTransformError], List[Block]]) = e match {
    case Left(value) => Left(value.toVector)
    case Right(value) => Right(value.toVector)
  }

  private def processMarkdownContainer(v: Vector[Block])(resultMap: Vector[Block] => Vector[Block]):
  Either[Vector[AstTransformError], Vector[Block]] = {
    def process(v: Vector[Block]) = transformTree(v) match {
      case Left(t) => Left(t)
      case Right(Ast(r)) => r.flatten |> transformTree match {
        case Left(errors) => Left(errors)
        case Right(Ast(blocks)) => Right(blocks.flatten)
      }
    }

    process(v) match {
      case l@Left(_) => l
      case Right(blocks) => Right(resultMap(blocks))
    }
  }

  private def processMarkdown(m: Markdown): Either[Vector[AstTransformError], Vector[Block]] = {
    def parse(inline: String): Either[String, Vector[Block]] = {
      val parser: BlockParser = new BlockParser(inline)
      parser.InputLine.run() match {
        case Success(node) => Right(node.toVector)
        case Failure(e: ParseError) => Left(parser.formatError(e, new ErrorFormatter(showTraces = true)))
        case Failure(e) => sys.error(e.getMessage)
      }
    }

    val Markdown(RawMarkdownContent(inline)) = m
    val (pre, cont) = inline splitToTuple "\u0000"

    pre |> parse match {
      case Left(value) => Left(Vector(value))
      case Right(value) if cont == "" => Right(value)
      case Right(firstValue) => Vector(Markdown(RawMarkdownContent(cont))) |> transformTree match {
        case Left(value) => Left(value)
        case Right(Ast(secondValue)) => Right((firstValue.toList ::: secondValue.flatten.toList).toVector)
      }
    }
  }

  private def processList(v: Vector[Block])(resultMap: Vector[Block] => Vector[Block]):
  Either[Vector[AstTransformError], Vector[Block]] = {
    def unwrap(b: Vector[Block]): Vector[Block] = b match {
      case Vector(UnorderedList(content)) => content flatMap (_ |> liftV |> unwrap)
      case Vector(OrderedList(content)) => content flatMap (_ |> liftV |> unwrap)
      case otherwise => otherwise
    }

    def process(blocks: Seq[Block]): Either[List[AstTransformError], List[Block]] = {
      def listProcess(x: Block, xs: List[Block]) = {
        sub(x) match {
          case Left(l) => Left(l.toList)
          case Right(rr) => process(xs) match {
            case Left(ll) => Left(ll)
            case Right(rrr) => Right((rr |> unwrap |> (_.toList)) ::: (rrr.toVector |> unwrap |> (_.toList)))
          }
        }
      }

      def sub(x: Block): Either[Vector[AstTransformError], Vector[Block]] = processMarkdownContainer(x |> liftV)(identity)

      blocks match {
        case x :: Nil => sub(x) match {
          case Left(value) => Left(value.toList)
          case Right(value) => Right(value |> unwrap |> (_.toList))
        }
        case x :: xs => listProcess(x, xs)
        case otherwise => Right(otherwise.toList)
      }
    }

    val parts: Vector[Block] = v.flatMap { case Markdown(RawMarkdownContent(ss)) => ss.split("\u0000").map(_.replaceAll("\u0000", "")).filter(_
      != "").map(c => Markdown(RawMarkdownContent(c)))
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
        map(processMarkdownContainer(_)(blocks => Vector(MultilineTableCell(blocks)))) |>
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

  private def transformNode(block: Block): Either[Vector[AstTransformError], Vector[Block]] = {
    block match {
      case Markdown(RawMarkdownContent("")) => Right(Vector(Plain(Vector(Text("")))))
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