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

  private def blockToHtml(node: Block): OutputContent = node match {
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
    case MultilineTableCell(inline) => tableCellToHtml(inline)
    case Markdown(_) => sys.error("Can't transform raw chunk of markdown to HTML. All nested markdown blocks must be " +
      "parsed before transforming AST tree to HTML output")
  }

  private def inlinesToHtml(inlines: InlineContent): RawContent = inlines.map(inlineToHtml).mkString

  private def processBlocks(blocks: Seq[Block]): Vector[OutputContent] = blocks.map(blockToHtml).toVector

  private def encloseInTagsSimple(openTag: HtmlTag): (RawContent) => OutputContent = encloseInTags("<" + openTag + ">", "</" + openTag + ">")

  private def encloseInTagsSimpleN(openTag: HtmlTag): (RawContent) => OutputContent = encloseInTags("<" + openTag + ">", "</" + openTag + ">" + EOL)

  private def encloseInTags(openTag: HtmlTag, closeTag: HtmlTag)(content: RawContent): OutputContent = openTag ++ content ++ closeTag

  private def selfClosingTagN(openTag: HtmlTag) = encloseInTags("</" + openTag + ">" + EOL, "")("")

  private def blockQuoteToHtml(inline: Vector[Block]) =
    inline |> processBlocks |> (_.mkString) |> encloseInTagsSimpleN("blockquote")

  private def tableCaptionToHtml(inline: Vector[Block], label: Option[String]) =
    inline |> processBlocks |> (_.mkString) |> encloseInTagsSimpleN("caption") // ToDo use label?

  private def tableCellToHtml(inline: Vector[Block]) = //ToDo validate
    inline |> processBlocks |> (_.mkString) |> encloseInTagsSimpleN("td")

  private def tableToHtml(relativeWidth: Vector[Float], caption: Option[MultilineTableCaption], head: Option[MultilineTableRow], body: Vector[MultilineTableColumn]) = {
    //ToDo apply relative width
    val maybeHead = head.
      map(b => b.map(bb => blockToHtml(bb) |> encloseInTagsSimpleN("th"))).
      map(head => head.mkString |> encloseInTagsSimpleN("tr") |> encloseInTagsSimpleN("thead")) // ToDo validate
    val maybeCaption = caption.map(blockToHtml(_))
    val tableBody = "" |> encloseInTagsSimpleN("tbody") //ToDo
    s"""${maybeCaption.getOrElse("")}${maybeHead.getOrElse("")}${tableBody}""" |> encloseInTagsSimpleN("table")
  }

  private def orderedListToHtml(inline: Vector[Block]) =
    inline |> processBlocks |> (_.map(c => c |> encloseInTagsSimpleN("li"))) |> (_.mkString) |> encloseInTagsSimpleN("ol")

  private def unorderedListToHtml(inline: Vector[Block]) =
    inline |> processBlocks |> (_.map(c => c |> encloseInTagsSimpleN("li"))) |> (_.mkString) |> encloseInTagsSimpleN("ul")
}