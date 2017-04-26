package com.mdpeg

sealed trait Block

final case class Plain(inline: String) extends Block
final case class Paragraph(inline: String) extends Block
case object HorizontalRuleBlock extends Block
final case class HeadingBlock(level: Int, inline: String) extends Block
final case class Verbatim(inline: String) extends Block
final case class BlockQuote(inline: String) extends Block
final case class TableBlock(inline: String) extends Block

// list cases
final case class OrderedListBlock(inline: Vector[Block]) extends Block
final case class UnorderedListBlock(inline: Vector[Block]) extends Block


/**
  * Raw markdown that is yet to be processed into blocks
  * @param inline - raw string
  */
final case class Markdown(inline: String) extends Block