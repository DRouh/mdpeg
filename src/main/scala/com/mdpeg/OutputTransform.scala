package com.mdpeg

import com.mdpeg.OutputTransform.inlinesToHtml

import scala.compat.Platform.EOL

object OutputTransform {
  type ErrorMessage = String
  type OutputContent = String
  type RawContent = String
  type HtmlTag = String
  type ReferenceMap = Map[InlineContent, (String, Option[String])]

  def toHtml(references: ReferenceMap)(astTree: Vector[Vector[Block]]): OutputContent =
    astTree.map(processBlocks(_, references).mkString).mkString

  private def inlineToHtml(i: Inline, references: ReferenceMap): RawContent = i match {
    case Code(inline) => inline |> encloseInTagsSimpleN("code")
    case i @ Image(_, _, _) => imageToHtml(i, references)
    case Strong(inline) => inline |> inlinesToHtml(references) |> encloseInTagsSimple("strong")
    case Italics(inline) => inline |> inlinesToHtml(references) |> encloseInTagsSimple("em")
    case l @ Link(_, _) => linkToHtml(l, references)
    case Text(inline) => inline
    case Space => " "
    case LineBreak => selfClosingTagN("br")
  }

  private def imageToHtml(i: Image, references: ReferenceMap) = {
    // ToDo handle other attributes
    //s"""<img alt="" src="" title="" width="${width.getOrElse(100)}%"/>"""
    val Image(l, t, w) = i
    t match {
      case Src(uri, None) =>""
      case Src(uri, Some(t)) => ???
      case Ref(label, ref) => ???
      case ShortcutRef => ???
    }
  }

  private def linkToHtml(link: Link, references: ReferenceMap) = {
    def toAnchor(uri:String) = encloseInTags(s"""<a href="${uri}">""", "</a>")
    def toAnchorWithTitle(uri:String, title:String) = encloseInTags(s"""<a href="${uri}"> title="${title}" """, "</a>")

    val Link(l, src) = link
    val content = l |> inlinesToHtml(references)

    src match {
      case Src(uri, None) => content |> toAnchor(uri)
      case Src(uri, Some(title)) => content |> toAnchorWithTitle(uri, title)
      case ShortcutRef =>
        references.get(l) match {
          case Some((uri, None)) => content |> toAnchor(uri)
          case Some((uri, Some(title))) => content |> toAnchorWithTitle(uri, title)
          case None => (Vector(Text("[")) ++ l ++ Vector(Text("]"))) |> inlinesToHtml(references)
        }
      case Ref(label, ref) =>
        val r = if (label.isEmpty) l else label.toVector
        references.get(r) match {
          case Some((uri, None)) => content |> toAnchor(uri)
          case Some((uri, Some(title))) => content |> toAnchorWithTitle(uri, title)
          case None => (Vector(Text("[")) ++ l ++ Vector(Text("]" + ref + "[")) ++ r ++ Vector(Text("]"))) |>
            inlinesToHtml(references)
        }
    }
  }

  private def blockToHtml(node: Block, references: ReferenceMap, isHead: Boolean = false): OutputContent = node match {
    case Plain(inline) => inline |> inlinesToHtml(references)
    case Paragraph(inline) => inline |> inlinesToHtml(references) |> encloseInTagsSimpleN("p")
    case HeadingBlock(level, inline) => inline |> inlinesToHtml(references) |> encloseInTagsSimple("h" + level)
    case BlockQuote(inline) => blockQuoteToHtml(inline, references)
    case UnorderedList(inline) => unorderedListToHtml(inline, references)
    case OrderedList(inline) => orderedListToHtml(inline, references)
    case Verbatim(inline) => inline |> encloseInTagsSimpleN("pre")
    case ReferenceBlock(_, _) => ""
    case HorizontalRuleBlock => selfClosingTagN("hr")
    case MultilineTableBlock(relativeWidth, caption, head, body) => tableToHtml(relativeWidth, caption, head, body, references)
    case MultilineTableCaption(inline, label) => tableCaptionToHtml(inline, label, references)
    case MultilineTableCell(inline) => tableCellToHtml(inline, isHead, references)
    case Markdown(_) => sys.error("Can't transform raw chunk of markdown to HTML. All nested markdown blocks must be " +
      "parsed before transforming AST tree to HTML output")
  }

  private def inlinesToHtml(references: ReferenceMap)(inlines: InlineContent): RawContent = inlines.map(inlineToHtml(_, references)).mkString

  private def processBlocks(blocks: Seq[Block], references: ReferenceMap): Vector[OutputContent] = blocks.map(blockToHtml(_, references)).toVector

  private def encloseInTagsSimple(openTag: HtmlTag): (RawContent) => OutputContent =
    encloseInTags("<" + openTag + ">", "</" + openTag + ">")

  private def encloseInTagsSimpleN(openTag: HtmlTag): (RawContent) => OutputContent =
    encloseInTags("<" + openTag + ">", "</" + openTag + ">" + EOL)

  private def encloseInTags(openTag: HtmlTag, closeTag: HtmlTag)(content: RawContent): OutputContent =
    openTag + content + closeTag

  private def selfClosingTagN(openTag: HtmlTag) = encloseInTags("</" + openTag + ">" + EOL, "")("")

  private def blockQuoteToHtml(inline: Vector[Block], references: ReferenceMap) =
    inline |> (processBlocks(_, references)) |> (_.mkString) |> encloseInTagsSimpleN("blockquote")

  private def tableCaptionToHtml(inline: Vector[Block], label: Option[String], references: ReferenceMap) =
    inline |> (processBlocks(_, references)) |> (_.mkString) |> encloseInTagsSimpleN("caption") // ToDo use label?

  private def tableCellToHtml(inline: Vector[Block], isHead: Boolean, references: ReferenceMap) = //ToDo validate
    inline |> (processBlocks(_, references)) |> (_.mkString) |> (if (isHead) tableCellHead else tableCellBody)

  private def tableToHtml(relativeWidth: Vector[Float], caption: Option[MultilineTableCaption],
                          head: Option[MultilineTableRow],
                          body: Vector[MultilineTableColumn],
                          references: ReferenceMap) = {

    //ToDo apply relative width
    val maybeHead = head.
      map(b => b.map(bb => blockToHtml(bb, references, isHead = true))).
      map(head => head.mkString |> tableRow |> encloseInTagsSimpleN("thead")).
      getOrElse("")

    val maybeCaption = caption.
      map(blockToHtml(_, references)).
      getOrElse("")

    val tableBody = body.
      map(_.toList).toList |>
      transposeAnyShape |>
      (_.map(processBlocks(_, references)).map(x => x.mkString |> tableRow).mkString) |>
      encloseInTagsSimpleN("tbody")
    s"""${maybeCaption}${maybeHead}${tableBody}""" |> tableWrapper
  }

  private def tableWrapper(content: String) = """<table style="border:1px solid black;border-collapse: collapse;">""" + content + "</table>"

  private def tableRow(content: String) = """<tr style="border:1px solid black;border-collapse: collapse;">""" + content + "</tr>"

  private def tableCellBody(content: String) = """<td style="border:1px solid black;border-collapse: collapse;">""" + content + "</td>"

  private def tableCellHead(content: String) = """<th style="border:1px solid black;border-collapse: collapse;">""" + content + "</th>"

  private def orderedListToHtml(inline: Vector[Block], references: ReferenceMap) =
    inline |> (processBlocks(_, references))  |> (_.map(c => c |> encloseInTagsSimpleN("li"))) |> (_.mkString) |> encloseInTagsSimpleN("ol")

  private def unorderedListToHtml(inline: Vector[Block], references: ReferenceMap) =
    inline |> (processBlocks(_, references))  |> (_.map(c => c |> encloseInTagsSimpleN("li"))) |> (_.mkString) |> encloseInTagsSimpleN("ul")
}