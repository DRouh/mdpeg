package org.mdpeg.ast

import org.mdpeg.InlineContent

sealed trait Inline

final case class Code(inline: CodeContent) extends Inline

final case class Image(inline: InlineContent, target: Target, width: Option[Int]) extends Inline

final case class Italics(inline: InlineContent) extends Inline

final case object LineBreak extends Inline

final case class Link(inline: InlineContent, target: Target) extends Inline

final case object Space extends Inline

final case class Strong(inline: InlineContent) extends Inline

final case class TexInline(inline: TexContent) extends Inline

final case class Text(inline: String) extends Inline