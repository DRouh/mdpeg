package org.mdpeg

sealed trait Block

final case class Plain(inline: InlineContent) extends Block
final case class Paragraph(inline: InlineContent) extends Block
final case class HeadingBlock(level: Int, inline: InlineContent) extends Block
final case class BlockQuote(inline: Vector[Block]) extends Block
final case class UnorderedList(inline: Vector[Block]) extends Block
final case class OrderedList(inline: Vector[Block]) extends Block
final case class Verbatim(inline: String) extends Block
final case class ReferenceBlock(inline: InlineContent, target: Target) extends Block
final case class Markdown(inline: String) extends Block { override def toString = s"""Markdown("${inline}")""" }
final case object HorizontalRuleBlock extends Block
final case class MultilineTableBlock(relativeWidth: Vector[Float],
                                     // ToDo capture alignment
                                     caption: Option[MultilineTableCaption],
                                     head: Option[MultilineTableRow],
                                     body: Vector[MultilineTableColumn]) extends Block

sealed trait MultilineTableElement
final case class MultilineTableCaption(inline: Vector[Block], label: Option[String]) extends MultilineTableElement with Block
final case class MultilineTableCell(inline: Vector[Block]) extends MultilineTableElement with Block

final case class TexBlock(inline: String) extends Block