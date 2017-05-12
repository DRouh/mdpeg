package com.mdpeg

sealed trait Block

final case class Plain(inline: String) extends Block
final case class Paragraph(inline: Seq[Inline]) extends Block
case object HorizontalRuleBlock extends Block
final case class HeadingBlock(level: Int, inline: String) extends Block
final case class Verbatim(inline: String) extends Block
final case class BlockQuote(inline: String) extends Block
final case class ReferenceBlock(inline: Seq[Inline], target: Target) extends Block

// list cases
final case class OrderedList(inline: Vector[Block]) extends Block
final case class UnorderedList(inline: Vector[Block]) extends Block

// multiline table
final case class MultilineTableBlock(relativeWidth: Vector[Float],
                                     caption: Option[MultilineTableCaption],
                                     head: Option[MultilineTableRow],
                                     body: Vector[MultilineTableColumn]) extends Block
sealed trait MultilineTableElement
case class MultilineTableCaption(inline: Markdown) extends MultilineTableElement
case class MultilineTableCell(inline: Markdown) extends MultilineTableElement
// ToDo capture alignment

/**
  * Raw markdown that is yet to be processed into blocks
  *
  * @param inline - raw string
  */
final case class Markdown(inline: String) extends Block