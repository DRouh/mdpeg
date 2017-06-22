package com.mdpeg

object OutputTransform {
  type ErrorMessage = String
  type OutputContent = String
  type RawContent = String
  type HtmlTag = String

  def encloseInTags(openTag:HtmlTag, closeTag:HtmlTag)(content: RawContent ): OutputContent = openTag ++ content ++ closeTag

  def inlineToHtml(i: Inline): RawContent = i match {
    case Code(inline) => ???
    case Image(inline, target, width) => ???
    case Strong(inline) => ???
    case Italics(inline) => inline |> inlinesToHtml |> encloseInTags("<i>", "</i>")
    case Link(inline, target) => inline |> inlinesToHtml |> encloseInTags(s"""<a href="${target}">""", "</a>")
    case Text(inline) => inline
    case Space => " "
    case LineBreak => "</br>"
  }

  def inlinesToHtml(inlines: InlineContent): RawContent = inlines.map(inlineToHtml).mkString

  def nodeToHtml(node: Block): OutputContent = node match {
    case Plain(inline) => inline |> inlinesToHtml |> encloseInTags("<p>", "</p>")
    case Paragraph(inline) => ???
    case HeadingBlock(level, inline) => ???
    case BlockQuote(inline) => ???
    case UnorderedList(inline) => ???
    case OrderedList(inline) => ???
    case Verbatim(inline) => ???
    case ReferenceBlock(inline, target) => ???
    case Markdown(inline) => ???
    case HorizontalRuleBlock => ???
    case MultilineTableBlock(relativeWidth, caption, head, body) => ???
    case MultilineTableCaption(inline, label) => ???
    case MultilineTableCell(inline) => ???
  }

  def toHtml(astTree: Vector[Block]): Either[ErrorMessage, OutputContent] = ???
}
