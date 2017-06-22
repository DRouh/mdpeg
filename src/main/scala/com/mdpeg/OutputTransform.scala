package com.mdpeg

object OutputTransform {
  type ErrorMessage = String
  type OutputContent = String
  type RawContent = String
  type HtmlTag = String

  def encloseInTags(openTag: HtmlTag, closeTag: HtmlTag)(content: RawContent): OutputContent =
    s"""${openTag}${content}${closeTag}"""

  def encloseInTagsSimple(openTag: HtmlTag): (RawContent) => OutputContent = encloseInTags(s""""<${openTag}>""",
    s"""</${openTag}>""")

  def selfClosingTag(openTag: HtmlTag) = encloseInTags(s""""<${openTag}/>""", s"")("")

  def inlineToHtml(i: Inline): RawContent = i match {
    case Code(inline) => inline |> encloseInTagsSimple("code")
    case Image(inline, target, width) => s"""<img alt="" src="" title="" width="${width.getOrElse(100)}%"/>""" //
    // ToDo handle other attributes
    case Strong(inline) => inline |> inlinesToHtml |> encloseInTagsSimple("strong")
    case Italics(inline) => inline |> inlinesToHtml |> encloseInTagsSimple("em")
    case Link(inline, target) => inline |> inlinesToHtml |> encloseInTags(s"""<a href="${target}">""", "</a>") //
    // ToDo handle other attributes
    case Text(inline) => inline |> encloseInTagsSimple("")
    case Space => " "
    case LineBreak => selfClosingTag("br")
  }

  def nodeToHtml(node: Block): OutputContent = node match {
    case Plain(inline) => inline |> inlinesToHtml
    case Paragraph(inline) => inline |> inlinesToHtml |> encloseInTagsSimple("p")
    case HeadingBlock(level, inline) => inline |> inlinesToHtml |> encloseInTagsSimple("h" + level)
    case BlockQuote(inline) => inline |> processBlocks |> (_.mkString) |> encloseInTagsSimple("blockquote")
    case UnorderedList(inline) =>
      inline |> processBlocks |> (_.map(c => c |> encloseInTagsSimple("li"))) |> (_.mkString) |> encloseInTagsSimple("ol")
    case OrderedList(inline) =>
      inline |> processBlocks |> (_.map(c => c |> encloseInTagsSimple("li"))) |> (_.mkString) |> encloseInTagsSimple("ol")
    case Verbatim(inline) => inline |> encloseInTagsSimple("pre")
    case ReferenceBlock(_, _) => ""
    case HorizontalRuleBlock => selfClosingTag("hr")
    case MultilineTableBlock(relativeWidth, caption, head, body) => //ToDo apply relative width
      val maybeHead = head.
        map(b => b.map(bb => nodeToHtml(bb) |> encloseInTagsSimple("th"))).
        map(head => head.mkString |> encloseInTagsSimple("tr") |> encloseInTagsSimple("thead")) // ToDo validate
      val maybeCaption = caption.map(nodeToHtml(_))
      val tableBody = "" |> encloseInTagsSimple("tbody") //ToDo
      s"""${maybeCaption.getOrElse("")}${maybeHead.getOrElse("")}${tableBody}""" |> encloseInTagsSimple("table")
    case MultilineTableCaption(inline, label) => inline |> processBlocks |> (_.mkString) |> encloseInTagsSimple("caption") // ToDo use label?
    case MultilineTableCell(inline) => inline |> processBlocks |> (_.mkString) |> encloseInTagsSimple("td") //ToDo validate
    case Markdown(_) => sys.error("Can't transform raw chunk of markdown to HTML. All nested markdown blocks must be " +
      "parsed before transforming AST tree to HTML output")
  }

  def inlinesToHtml(inlines: InlineContent): RawContent = inlines.map(inlineToHtml).mkString

  def processBlocks(blocks: Seq[Block]): Vector[OutputContent] = blocks.map(nodeToHtml).toVector

  def toHtml(astTree: Vector[Block]): Either[ErrorMessage, OutputContent] = ???
}