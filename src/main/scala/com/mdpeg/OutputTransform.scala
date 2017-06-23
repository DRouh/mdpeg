package com.mdpeg

import scala.compat.Platform.EOL

object OutputTransform {
  type ErrorMessage = String
  type OutputContent = String
  type RawContent = String
  type HtmlTag = String

  def toHtml(astTree: Vector[Vector[Block]]): OutputContent = astTree.map(processBlocks(_).mkString).mkString

  private def inlineToHtml(i: Inline): RawContent = i match {
    case Code(inline) => inline |> encloseInTagsSimpleN("code")
    case Image(inline, target, width) => s"""<img alt="" src="" title="" width="${width.getOrElse(100)}%"/>""" //
    // ToDo handle other attributes
    case Strong(inline) => inline |> inlinesToHtml |> encloseInTagsSimple("strong")
    case Italics(inline) => inline |> inlinesToHtml |> encloseInTagsSimple("em")
    case Link(inline, target) => inline |> inlinesToHtml |> encloseInTags(s"""<a href="${target}">""", "</a>") //
    // ToDo handle other attributes
    case Text(inline) => inline
    case Space => " "
    case LineBreak => selfClosingTagN("br")
  }

  private def blockToHtml(node: Block, isHead: Boolean = false): OutputContent = node match {
    case Plain(inline) => inline |> inlinesToHtml
    case Paragraph(inline) => inline |> inlinesToHtml |> encloseInTagsSimpleN("p")
    case HeadingBlock(level, inline) => inline |> inlinesToHtml |> encloseInTagsSimple("h" + level)
    case BlockQuote(inline) => blockQuoteToHtml(inline)
    case UnorderedList(inline) => unorderedListToHtml(inline)
    case OrderedList(inline) => orderedListToHtml(inline)
    case Verbatim(inline) => inline |> encloseInTagsSimpleN("pre")
    case ReferenceBlock(_, _) => ""
    case HorizontalRuleBlock => selfClosingTagN("hr")
    case MultilineTableBlock(relativeWidth, caption, head, body) => tableToHtml(relativeWidth, caption, head, body)
    case MultilineTableCaption(inline, label) => tableCaptionToHtml(inline, label)
    case MultilineTableCell(inline) => tableCellToHtml(inline, isHead)
    case Markdown(_) => sys.error("Can't transform raw chunk of markdown to HTML. All nested markdown blocks must be " +
      "parsed before transforming AST tree to HTML output")
  }

  private def inlinesToHtml(inlines: InlineContent): RawContent = inlines.map(inlineToHtml).mkString

  private def processBlocks(blocks: Seq[Block]): Vector[OutputContent] = blocks.map(blockToHtml(_)).toVector

  private def encloseInTagsSimple(openTag: HtmlTag): (RawContent) => OutputContent =
    encloseInTags("<" + openTag + ">", "</" + openTag + ">")

  private def encloseInTagsSimpleN(openTag: HtmlTag): (RawContent) => OutputContent =
    encloseInTags("<" + openTag + ">", "</" + openTag + ">" + EOL)

  private def encloseInTags(openTag: HtmlTag, closeTag: HtmlTag)(content: RawContent): OutputContent =
    openTag + content + closeTag

  private def selfClosingTagN(openTag: HtmlTag) = encloseInTags("</" + openTag + ">" + EOL, "")("")

  private def blockQuoteToHtml(inline: Vector[Block]) =
    inline |> processBlocks |> (_.mkString) |> encloseInTagsSimpleN("blockquote")

  private def tableCaptionToHtml(inline: Vector[Block], label: Option[String]) =
    inline |> processBlocks |> (_.mkString) |> encloseInTagsSimpleN("caption") // ToDo use label?

  private def tableCellToHtml(inline: Vector[Block], isHead: Boolean) = //ToDo validate
    inline |> processBlocks |> (_.mkString) |> (if (isHead) tableCellHead else tableCellBody)

  private def tableToHtml(relativeWidth: Vector[Float], caption: Option[MultilineTableCaption],
                          head: Option[MultilineTableRow],
                          body: Vector[MultilineTableColumn]) = {

    //ToDo apply relative width
    val maybeHead = head.
      map(b => b.map(bb => blockToHtml(bb, isHead = true))).
      map(head => head.mkString |> tableRow |> encloseInTagsSimpleN("thead")).
      getOrElse("")

    val maybeCaption = caption.
      map(blockToHtml(_)).
      getOrElse("")

    val tableBody = body.
      map(_.toList).toList |>
      transposeAnyShape |>
      (_.map(processBlocks(_)).map(x => x.mkString |> tableRow).mkString) |>
      encloseInTagsSimpleN("tbody")
    s"""${maybeCaption}${maybeHead}${tableBody}""" |> tableWrapper
  }

  private def tableWrapper(content: String) = """<table style="border:1px solid black;border-collapse: collapse;">""" + content + "</table>"

  private def tableRow(content: String) = """<tr style="border:1px solid black;border-collapse: collapse;">""" + content + "</tr>"

  private def tableCellBody(content: String) = """<td style="border:1px solid black;border-collapse: collapse;">""" + content + "</td>"

  private def tableCellHead(content: String) = """<th style="border:1px solid black;border-collapse: collapse;">""" + content + "</th>"

  private def orderedListToHtml(inline: Vector[Block]) =
    inline |> processBlocks |> (_.map(c => c |> encloseInTagsSimpleN("li"))) |> (_.mkString) |> encloseInTagsSimpleN("ol")

  private def unorderedListToHtml(inline: Vector[Block]) =
    inline |> processBlocks |> (_.map(c => c |> encloseInTagsSimpleN("li"))) |> (_.mkString) |> encloseInTagsSimpleN("ul")
}