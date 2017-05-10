package com.mdpeg

sealed trait Inline
case class Strong(inline: Seq[Inline]) extends Inline
case class Italics(inline: String) extends Inline //ToDo should be Inline instead of String
case class Text(inline: String) extends Inline
case object Space extends Inline
case object LineBreak extends Inline