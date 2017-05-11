package com.mdpeg

sealed trait Inline
case class Image(inline: Seq[Inline], target: Target, width: Option[Int]) extends Inline
case class Strong(inline: Seq[Inline]) extends Inline
case class Italics(inline: Seq[Inline]) extends Inline
case class Link(inline: Seq[Inline], target: Target) extends Inline
case class Text(inline: String) extends Inline //{ override def toString = s"""Text("${inline}")""" }
case object Space extends Inline
case object LineBreak extends Inline