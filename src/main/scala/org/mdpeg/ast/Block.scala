package org.mdpeg.ast

import org.mdpeg.{InlineContent, MultilineTableColumn, MultilineTableRow}

sealed trait Block

final case class BlockQuote(inline: Vector[Block]) extends Block

final case class HeadingBlock(level: Int, inline: InlineContent) extends Block

final case object HorizontalRuleBlock extends Block

final case class Markdown(inline: RawMarkdownContent) extends Block

final case class MultilineTableBlock(relativeWidth: Vector[Float], caption: Option[MultilineTableCaption],
                                     head: Option[MultilineTableRow], body: Vector[MultilineTableColumn]) extends Block

final case class MultilineTableCaption(inline: Vector[Block], label: Option[String]) extends Block

final case class MultilineTableCell(inline: Vector[Block]) extends Block

final case class OrderedList(inline: Vector[Block]) extends Block

final case class Paragraph(inline: InlineContent) extends Block

final case class Plain(inline: InlineContent) extends Block

final case class ReferenceBlock(inline: InlineContent, target: Target) extends Block

final case class TexBlock(inline: TexContent) extends Block

final case class UnorderedList(inline: Vector[Block]) extends Block

final case class Verbatim(inline: String, syntax: Option[String]) extends Block