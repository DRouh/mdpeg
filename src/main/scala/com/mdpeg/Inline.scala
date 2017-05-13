package com.mdpeg

sealed trait Inline
case class Code(inline: String) extends Inline
case class Image(inline: InlineContent, target: Target, width: Option[Int]) extends Inline
case class Strong(inline: InlineContent) extends Inline
case class Italics(inline: InlineContent) extends Inline
case class Link(inline: InlineContent, target: Target) extends Inline
case class Text(inline: String) extends Inline //{ override def toString = s"""Text("${inline}")""" }
case object Space extends Inline
case object LineBreak extends Inline