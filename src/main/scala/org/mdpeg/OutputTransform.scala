package org.mdpeg

import org.mdpeg.ast._

import scala.compat.Platform.EOL

private[mdpeg] object OutputTransform {
  type HtmlContent = String
  type RawContent = String
  type HtmlTag = String
  type ReferenceMap = Map[InlineContent, (String, Option[String])]

  private def toUpper(s: InlineContent) : InlineContent = s.map {
    case Text(i) => Text(i.toUpperCase)
    case otherwise => otherwise
  }

  private def lookupInReferences(references: ReferenceMap)(lookup: InlineContent) =
    lookup |> toUpper |> references.get

  def toHtml(references: ReferenceMap)(astTree: Ast): HtmlContent = {
    val refs = references.map{ case (i, (s, oS)) => (i |> toUpper, (s, oS)) }.toMap
    astTree.map(processBlocks(refs)(_).mkString).mkString
  }

  private def toLatexInline(inline: String): RawContent = s"""<span lang="latex">$inline</span>"""

  private def inlineToHtml(references: ReferenceMap)(i: Inline): RawContent = i match {
    case Code(CodeContent(inline)) => inline |> encloseInTagsSimpleN("code")
    case i @ Image(_, _, _) => i |> imageToHtml(references)
    case Strong(inline) => inline |> inlinesToHtml(references) |> encloseInTagsSimple("strong")
    case Italics(inline) => inline |> inlinesToHtml(references) |> encloseInTagsSimple("em")
    case l @ Link(_, _) => l |> linkToHtml(references)
    case Text(inline) => inline
    case TexInline(TexContent(inline)) => toLatexInline(inline)
    case Space => " "
    case LineBreak => selfClosingTagN("br")
  }

  private def imageToHtml(references: ReferenceMap)(i: Image): RawContent = {
    val Image(label, title, w) = i
    val width = w.map(c => s"""width="$c%"""").getOrElse("")
    title match {
      case Src(uri, None) =>
        val alt = label |> inlinesToHtml(references)
        s"""<img alt="$alt" src="$uri" $width />"""
      case Src(uri, Some(t)) =>
        val alt = label |> inlinesToHtml(references)
        s"""<img alt="$alt" src="$uri" title="$t" $width />"""
      case ShortcutRef =>
        label |> lookupInReferences(references) match {
          case Some((uri, t)) => Image(label, Src(uri, t), w) |> imageToHtml(references)
          case None => (Vector(Text("![")) ++ label ++ Vector(Text("]"))) |> inlinesToHtml(references)
        }
      case Ref(l, ref) =>
        val r = if (l.isEmpty) l else l.toVector
        r |> lookupInReferences(references) match {
          case Some((uri, t)) => Image(l, Src(uri, t), w) |> imageToHtml(references)
          case None => (Vector(Text("![")) ++ l ++ Vector(Text("]" + ref + "[")) ++ r ++ Vector(Text("]"))) |>
            inlinesToHtml(references)
        }
    }
  }

  private def linkToHtml(references: ReferenceMap)(link: Link) = {
    def toAnchor(uri: String) =
      encloseInTags(s"""<a href="$uri">""", "</a>") _

    def toAnchorWithTitle(uri: String, title: String) =
      encloseInTags(s"""<a href="$uri"> title="$title" """, "</a>") _

    val Link(label, src) = link
    val content = label |> inlinesToHtml(references)

    src match {
      case Src(uri, None) => content |>
        toAnchor(uri)
      case Src(uri, Some(title)) => content |>
        toAnchorWithTitle(uri, title)
      case ShortcutRef =>
        label |> lookupInReferences(references) match {
          case Some((uri, None)) =>
            content |> toAnchor(uri)
          case Some((uri, Some(title))) =>
            content |> toAnchorWithTitle(uri, title)
          case None =>
            (Vector(Text("[")) ++ label ++ Vector(Text("]"))) |> inlinesToHtml(references)
        }
      case Ref(l, ref) =>
        val r = if (l.isEmpty) l else l.toVector
        r |> lookupInReferences(references) match {
          case Some((uri, None)) => content |> toAnchor(uri)
          case Some((uri, Some(title))) => content |> toAnchorWithTitle(uri, title)
          case None => (Vector(Text("[")) ++ l ++ Vector(Text("]" + ref + "[")) ++ r ++ Vector(Text("]"))) |>
            inlinesToHtml(references)
        }
    }
  }

  def toTexBlock(inline: String): HtmlContent =
    s"""<div lang="latex">
      |$inline
      |</div>
      |""".stripMargin

  private def blockToHtml(references: ReferenceMap)(node: Block, isHead: Boolean = false): HtmlContent = node match {
    case Plain(inline) => inline |> inlinesToHtml(references)
    case Paragraph(inline) => inline |> inlinesToHtml(references) |> encloseInTagsSimpleN("p")
    case HeadingBlock(level, inline) => inline |> inlinesToHtml(references) |> encloseInTagsSimple("h" + level)
    case BlockQuote(inline) => inline |> blockQuoteToHtml(references)
    case UnorderedList(inline) => inline |> unorderedListToHtml(references)
    case OrderedList(inline) => inline |> orderedListToHtml(references)
    case Verbatim(inline, _) => inline |> encloseInTagsSimpleN("pre")
    case TexBlock(TexContent(inline)) => toTexBlock(inline)
    case ReferenceBlock(_, _) => ""
    case HorizontalRuleBlock => selfClosingTagN("hr")
    case table @ MultilineTableBlock(_, _, _, _) => table |> tableToHtml(references)
    case caption @ MultilineTableCaption(_, _) => caption |> tableCaptionToHtml(references)
    case MultilineTableCell(inline) => tableCellToHtml(references)(inline, isHead)
    case Markdown(_) => sys.error("Can't transform raw chunk of markdown to HTML. All nested markdown blocks must be " +
      "parsed before transforming AST tree to HTML output")
  }

  private def inlinesToHtml(references: ReferenceMap)(inlines: InlineContent): RawContent =
    inlines.map(inlineToHtml(references)).mkString

  private def processBlocks(references: ReferenceMap)(blocks: Seq[Block]): Vector[HtmlContent] =
    blocks.map(blockToHtml(references)(_)).toVector

  private def encloseInTagsSimple(openTag: HtmlTag): (RawContent) => HtmlContent =
    encloseInTags("<" + openTag + ">", "</" + openTag + ">")

  private def encloseInTagsSimpleN(openTag: HtmlTag): (RawContent) => HtmlContent =
    encloseInTags("<" + openTag + ">", "</" + openTag + ">" + EOL)

  private def encloseInTags(openTag: HtmlTag, closeTag: HtmlTag)(content: RawContent): HtmlContent =
    openTag + content + closeTag

  private def selfClosingTagN(openTag: HtmlTag) = encloseInTags("<" + openTag + " />" + EOL, "")("")

  private def blockQuoteToHtml(references: ReferenceMap)(inline: Vector[Block]) =
    inline |> processBlocks(references) |> (_.mkString) |> encloseInTagsSimpleN("blockquote")

  private def tableCaptionToHtml(references: ReferenceMap)(caption: MultilineTableCaption) = {
    val MultilineTableCaption(inline, _) = caption // ToDo use label?
    inline |> processBlocks(references) |> (_.mkString) |> encloseInTagsSimpleN("caption")
  }

  private def tableCellToHtml(references: ReferenceMap)(inline: Vector[Block], isHead: Boolean) = //ToDo validate
    inline |> processBlocks(references) |> (_.mkString) |> (if (isHead) tableCellHead else tableCellBody)

  private def tableToHtml(references: ReferenceMap)(table: MultilineTableBlock) = {
    val MultilineTableBlock(relativeWidth, caption, head, body) = table //ToDo apply relative width

    val maybeHead = head.
      map(b => b.map(bb => blockToHtml(references)(bb, isHead = true))).
      map(head => head.mkString |> tableRow |> encloseInTagsSimpleN("thead")).
      getOrElse("")

    val maybeCaption = caption.
      map(blockToHtml(references)(_)).
      getOrElse("")

    val tableBody = body.
      map(_.toList).toList |>
      transposeAnyShape |>
      (_.map(processBlocks(references)).map(x => x.mkString |> tableRow).mkString) |>
      encloseInTagsSimpleN("tbody")

    val colgroup = relativeWidth.
      map(w => selfClosingTagN(s"""col width="$w%"""")).mkString |>
      encloseInTagsSimpleN("colgroup")
    s"""$maybeCaption$colgroup$maybeHead$tableBody""" |> tableWrapper
  }

  private def tableWrapper(content: String) =
    """<table style="border:1px solid black;border-collapse: collapse;">""" + content + "</table>"

  private def tableRow(content: String) =
    """<tr style="border:1px solid black;border-collapse: collapse;">""" + content + "</tr>"

  private def tableCellBody(content: String) =
    """<td style="border:1px solid black;border-collapse: collapse;">""" + content + "</td>"

  private def tableCellHead(content: String) =
    """<th style="border:1px solid black;border-collapse: collapse;">""" + content + "</th>"

  private def orderedListToHtml(references: ReferenceMap)(inline: Vector[Block]) = inline |>
    processBlocks(references) |> (_.map(encloseInTagsSimpleN("li")(_))) |> (_.mkString) |> encloseInTagsSimpleN("ol")

  private def unorderedListToHtml(references: ReferenceMap)(inline: Vector[Block]) = inline |>
    processBlocks(references) |> (_.map(encloseInTagsSimpleN("li")(_))) |> (_.mkString) |> encloseInTagsSimpleN("ul")
}